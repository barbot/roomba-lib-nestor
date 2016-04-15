open Type_def

type roomba =  {
  serial: Unix.file_descr;
  state: Type_def.roomba_state;
  state_mutex: Mutex.t;
  synchronise: Mutex.t;
  mutable stop_flag: bool;
  mutable callback: Type_def.roomba_state -> unit;
  mutable thread_sync: Thread.t option;
}

let dummy_roomba () = {
  serial = Unix.stdout;
  state = Type_def.emptystate;
  state_mutex = Mutex.create ();
  synchronise = Mutex.create ();
  stop_flag = false;
  callback = (fun _ -> ());
  thread_sync = None 
}

let get_state ro = 
  Mutex.lock ro.state_mutex;
  let s = {ro.state with Type_def.wall = ro.state.Type_def.wall } in
  Mutex.unlock ro.state_mutex;
  s

let debug = false

let soc i =
  let s = Bytes.create 1 in
  Bytes.set s 0 (char_of_int i);
  s

let convertbyte x =
  let high = char_of_int ((x lsr 8) land 255)
  and low = char_of_int (x land 255) in
  Printf.sprintf "%c%c" high low

let roomba_cmd_string = function
  | Start -> soc 128
  | Control -> soc 130
  | Safe -> soc 131
  | Full -> soc 132
  | Power -> soc 133
  | Spot -> soc 134
  | Clean -> soc 135
  | Max -> soc 136
  | Drive (speed,rad) -> Printf.sprintf "%s%s%s" (soc 137) (convertbyte speed) (convertbyte rad)
  | Motors m -> Printf.sprintf "%s%c" (soc 138) (char_of_int m)
  | Leds (lb,pc,pi) ->
    Printf.sprintf "%s%c%c%c" (soc 139) (char_of_int lb) (char_of_int pc) (char_of_int pi)
  | Dock -> soc 143
  | DriveDirect (lw,rw) -> Printf.sprintf "%s%s%s" (soc 145) (convertbyte rw) (convertbyte lw)
  | Stream l -> List.fold_left (fun x y -> Printf.sprintf "%s%c" x (char_of_int y)) ("\148"^(soc (List.length l))) l
  | QueryList l -> List.fold_left (fun x y -> Printf.sprintf "%s%c" x (char_of_int y)) ("\149"^(soc (List.length l))) l
  | PauseStream v -> Printf.sprintf "%s%c" (soc 150) (char_of_int (if v then 1 else 0))
  | WakeUp ->
     ignore (Sys.command "echo 0 > /sys/class/gpio/gpio4/value");
    ignore (Unix.select [] [] [] 0.01);
    ignore (Sys.command "echo 1 > /sys/class/gpio/gpio4/value");
    ""
     

let roomba_cmd r cmd =
  let s = roomba_cmd_string cmd in
  let n = String.length s in
  if (Unix.write r.serial s 0 n) != n then failwith "Communication error"


let rec clear_input ro =
  try 
    let (l2,_,_) = Unix.select [ro.serial] [] [] 0.0 in
    let l = ref l2 in
    let buff = Bytes.create 256 in
    while !l <> [] do 
      ignore (Unix.read ro.serial buff 0 256);
      let (l2,_,_) = Unix.select [ro.serial] [] [] 0.0 in
      l := l2;
    done
  with
      Unix.Unix_error(Unix.EAGAIN,_,_) -> clear_input ro
    | Unix.Unix_error(Unix.EINTR,_,_) -> clear_input ro
    | x -> raise x 

let read_t fd s i1 i2 time =
  let rl,_,_ = Unix.select [fd] [] [] time in
  if rl == [] then -1
  else Unix.read fd s i1 i2


let fireflyfastmode serial =
  let s1 = "$$$" 
  and s2 = "F,1\r" in
  if (Unix.write serial s1 0 3) <> 3 then failwith "IO error";
  let i1 = "    " in
  let rn = read_t serial i1 0 4 3.0 in
  if rn = 4 then begin
    print_endline i1;
    if (Unix.write serial s2 0 4) <> 4 then failwith "IO error";
    print_endline "FireFly Fast Mode";
  end else if rn = -1 then print_endline "FireFly timout"
    
  (*let s2 = String.create 64 in
  ignore (Unix.read serial s2 0 64);
  print_endline s2*)

let open_connection s =
  let serial = Unix.openfile s [Unix.O_RDWR;Unix.O_NOCTTY] 0o666 in
  let term = Unix.tcgetattr serial in 
  Unix.tcflush serial Unix.TCIOFLUSH;
  Unix.tcsetattr
    serial
    Unix.TCSANOW
    {term with 
Unix.c_ibaud =  115200 ;
Unix.c_obaud =  115200 ;

      Unix.c_icanon = false;
      Unix.c_echo = false;
      Unix.c_echoe = false;
      Unix.c_echok = false;
      Unix.c_echonl = false;
      Unix.c_isig = false;
      
      Unix.c_inlcr = false;
      Unix.c_igncr = false;
      Unix.c_icrnl = false;
      Unix.c_ignbrk = false;
      Unix.c_parmrk = false;

      Unix.c_ixon = false;
      Unix.c_ixoff = false;

      Unix.c_cread =true;
      Unix.c_clocal=true;

      Unix.c_csize = 8;
      Unix.c_parenb = false ;
      Unix.c_parodd = false ;
      Unix.c_inpck = false;
      Unix.c_istrip = false;

      Unix.c_cstopb = 1;
      Unix.c_opost = false ;
      Unix.c_vmin = 1;
      Unix.c_vtime =0;
      
      
    (*  Unix.c_hupcl = false;*)
      
      };
  (*fireflyfastmode serial;*)
  serial

exception Wrong_checksum

let checksum s =
  let n = ref 19 in
  for i =0 to String.length s-1 do
    n := !n + (int_of_char s.[i]);
  done;
  ( !n land 255 = 0 )


let rec read_streampacket ro s i =
  if debug then Printf.printf "Read stream packet at character %i\n" i;
  if i = String.length s -1 then ()
  else 
    let packetnb = int_of_char s.[i] in
    if debug then Printf.printf "Try to import packet %i\n" packetnb;
    Import_packet.import_packet ro.state s (i+1) packetnb;
    read_streampacket ro s (i+1+ Import_packet.packet_length packetnb)

let print_chars s =
  for i =0 to String.length s -1 do
    print_string "[";
    print_int (int_of_char s.[i]);
    print_string "=";
    print_char s.[i];
    print_string "]";
  done

let rec readserial serial s ofs n =
  if n > 0 then (
    if debug then Printf.printf "Try to read %i octet\n" n;
    let nread =  Unix.read serial s ofs n in
    if nread = 0 then raise End_of_file;
    if debug then (Printf.printf "%i octets reads:" nread;
		   print_chars (String.sub s ofs nread);
		   print_newline ());
    readserial serial s (ofs+nread) (n-nread))

let read_single_packet ro i =
  let n = Import_packet.packet_length i in
  let s = Bytes.create (n+1) in
  if debug then Printf.printf "Try to read packet %i of size %i\n" i n;
  readserial ro.serial s 0 n;
  if debug then Printf.printf "Read sucessfull\n";
  ignore (Import_packet.import_packet ro.state s 0 i)

let read_stream ro =
  Type_def.clear_state ro.state;
  try
    let head = " " in
    readserial ro.serial head 0 1;
    if int_of_char head.[0] <> 19 then begin
      if debug then 
	print_string "Wrong header:";
	print_chars head;
	print_newline ()
    end else let head2 = " " in 
	 if debug then (print_newline ();
			   print_endline "new packet");
	 readserial ro.serial head2 0 1;
	 let nbyte = int_of_char head2.[0] in
	 let stream = Bytes.create (nbyte+2) in
	 Bytes.set stream 0 (head2.[0]);
	 readserial ro.serial stream 1 (nbyte+1);
	 if debug then (print_chars stream;
			  print_newline ());
	 if not (checksum stream) then raise Wrong_checksum;
	 read_streampacket ro stream 1
  with
      exn -> raise exn

let aquire_or_die ro =
  if not (Mutex.try_lock ro.synchronise) then
    raise Type_def.Upstream_in_use

let query_list ro l =
  aquire_or_die ro;
  roomba_cmd ro (QueryList l);
  Type_def.clear_state ro.state;
  List.iter (fun x -> read_single_packet ro x) l;
  Mutex.unlock ro.synchronise

let sync_state ro l = 
  aquire_or_die ro;
  ro.stop_flag<-false;
  clear_input ro;
  let rec sync () = while not ro.stop_flag do
    try 
      let serlist,_,_ = Unix.select [ro.serial] [] [] (0.001) in
      if serlist <> [] then (
	Mutex.lock ro.state_mutex;
	begin
	  try read_stream ro with
	      Wrong_checksum -> if debug then print_endline "Wrong checksum"
	    | x -> raise x
	end ;
	Mutex.unlock ro.state_mutex;
	Type_def.update_time ro.state;
	ro.callback ro.state)
    with 
	Unix.Unix_error(Unix.EAGAIN,_,_) -> sync ()
      | Unix.Unix_error(Unix.EINTR,_,_) -> sync ()
      | x -> raise x
    done
  in
  let t = Thread.create (fun () ->
    roomba_cmd ro (Stream l);
    (try sync () with x -> (
      roomba_cmd ro (PauseStream false);
      Mutex.unlock ro.synchronise;
      print_endline "Synchronisation stopped due to exception";
      raise x));
    roomba_cmd ro (PauseStream false);
    clear_input ro;
    Mutex.unlock ro.synchronise;
    print_endline "Synchronisation stopped"
  ) () in ro.thread_sync <- Some t
	
let stop_sync ro =
  ro.stop_flag<-true;
  Mutex.lock ro.synchronise;
  Mutex.unlock ro.synchronise    

let change_callback ro f =
  ro.callback <- f

let wait_end_callback ro =
  match ro.thread_sync with
      None -> ()
    | Some t -> Thread.join t

let close_roomba ro = 
  stop_sync ro;
  roomba_cmd ro (Drive (0,0));
  roomba_cmd ro (Motors 0);
  roomba_cmd ro (PauseStream false);
  clear_input ro;
  Unix.close ro.serial

let init_roomba s = 
  let serial = open_connection s in
  let roomba = {
    serial = serial;
    state = {Type_def.emptystate with Type_def.wall = None};
    state_mutex = Mutex.create ();
    synchronise = Mutex.create ();
    stop_flag = false;
    callback = (fun _ -> ());
    thread_sync = None } in
  roomba_cmd roomba Start;
  roomba_cmd roomba (PauseStream false);
  clear_input roomba;
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _->
    roomba_cmd roomba (Drive (0,0));
    roomba_cmd roomba (PauseStream false);
    clear_input roomba;
    exit 130));
  roomba
