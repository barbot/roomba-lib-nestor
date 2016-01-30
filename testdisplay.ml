open Interface

let rec sleepl i =
  try ignore (Unix.select [] [] [] (float (i*1000))) with
      Unix.Unix_error(Unix.EAGAIN,_,_) -> 
	sleepl i
    | Unix.Unix_error(Unix.EINTR,_,_) -> 
      sleepl i
    | Unix.Unix_error(Unix.EUNKNOWNERR(260),_,_) -> 
      sleepl i; 
    | Unix.Unix_error(j,_,_) -> 
      print_endline "uncaught sleep";
      print_int (Obj.magic j);
      sleepl i; 
    | x -> print_endline "uncaught sleep";
      raise x

let callbackcmd nestor s =
  Display.callbackfun s;
  if Graphics.key_pressed ()
  then let v = Graphics.read_key () in
       Printf.printf "Key %c pressed" v;
       print_newline ();
       match v with
      'z' -> roomba_cmd nestor (Drive (100,0))
    | 's' -> roomba_cmd nestor (Drive (0,0))
    | 'q' -> roomba_cmd nestor (Drive (100,1))
    | 'd' -> roomba_cmd nestor (Drive (100,-1))
    | 'c' -> roomba_cmd nestor Clean
    | 'm' -> roomba_cmd nestor (Motors 7)
    | 'l' -> roomba_cmd nestor (Motors 0)
    | 'a' -> failwith "User Quit"
    | _-> ()

let simplecallback _ s =
  print_endline (Type_def.string_of_available_data s)

let test () =
  let nestor = init_roomba Sys.argv.(1) in
  (*let nestor = init_roomba "/dev/tty.Nestor-SPP-4" in*)

  try 
    roomba_cmd nestor Safe;

    print_endline "Start";
    Display.init ();
    Display.echelle := 0.1;
    Display.origine := (5000. , 3000.);

    sync_state nestor [1;2;3;43;44];
    print_endline "Sync";
    
    (*change_callback nestor (simplecallback nestor);*)
    change_callback nestor (callbackcmd nestor);
    print_endline "call back";
    
    (*roomba_cmd nestor Clean;
    print_endline "clean";*)

    wait_end_callback nestor;


    close_roomba nestor;
  with 
      x -> close_roomba nestor; raise x;;

test ();
