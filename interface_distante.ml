open Type_def
open Interface_server

type roomba =  {
  in_chan: in_channel;
  out_chan: out_channel;
  mutable state: Type_def.roomba_state;
  state_mutex: Mutex.t;
  synchronise: Mutex.t;
  mutable stop_flag: bool;
  mutable callback: Type_def.roomba_state -> unit;
  mutable thread_sync: Thread.t option;
}

let dummy_roomba () = {
  in_chan= stdin;
  out_chan= stdout;
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

let output_order ro order =
  output_value ro.out_chan order;
  flush ro.out_chan

let roomba_cmd ro cmd =
  output_order ro (Roomba_cmd cmd)

let clear_input ro =
  output_order ro (Clear_input)

let open_connection s =
  let is = try String.index s ':' with Not_found -> String.length s in
  let host = Unix.gethostbyname (String.sub s 0 is) in
  let port = if is < String.length s then 
      int_of_string (String.sub s (is+1) (String.length s -is -1)) 
    else 8079 in
  Unix.open_connection (Unix.ADDR_INET(host.Unix.h_addr_list.(0),port))

exception Wrong_checksum

let aquire_or_die ro =
  if not (Mutex.try_lock ro.synchronise) then
    raise Type_def.Upstream_in_use

let query_list ro l =
  aquire_or_die ro;
  output_order ro (Query_list l);
  ro.state <- input_value ro.in_chan;
  Mutex.unlock ro.synchronise

let sync_state ro l = 
  aquire_or_die ro;
  ro.stop_flag<-false;
  let rec sync () = 
    let t = input_value ro.in_chan in
    if ro.stop_flag then ()
    else begin
      ro.state <- t;
      Type_def.update_time ro.state;
      ro.callback ro.state;
      sync ()
    end
  in
  let t = Thread.create (fun () ->
    output_order ro (Sync_state l);
    (try sync () with x -> (
      output_order ro Stop_sync;
      Mutex.unlock ro.synchronise;
      print_endline "Synchronisation stopped due to exception";
      raise x));
    output_order ro Stop_sync;
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
  Unix.shutdown_connection ro.in_chan

let init_roomba s = 
  let is = try String.index s '/' with Not_found -> String.length s in
  let inc,outc = open_connection (String.sub s 0 is) in
  output_value outc (String.sub s is (String.length s -is));
  flush outc;
  begin match input_value inc with
    | None -> ()
    | Some x -> raise x;
  end;
  let roomba = {
    in_chan = inc;
    out_chan =outc;
    state = {Type_def.emptystate with Type_def.wall = None};
    state_mutex = Mutex.create ();
    synchronise = Mutex.create ();
    stop_flag = false;
    callback = (fun _ -> ());
    thread_sync = None } in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _->
    roomba_cmd roomba (Drive (0,0));
    roomba_cmd roomba (PauseStream false);
    exit 130));
  roomba
