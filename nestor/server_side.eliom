[%%shared
  (* Modules opened in the shared-section are available in client-
     and server-code *)
   open Lwt

  type order =
      Wakeup | Refresh | Close | Synchronize | Stop_syn
    | Safe | Move of int*int
    | Clean | Power | Spot | Max | Dock
	[@@deriving json]

  type server_state =
      Disconnected
    | Connected of bool 
    | Synchronized of bool
	[@@deriving json]

  type messages =
    float*float*float *
      ( (string*string) list)
      [@@deriving json]

]

open Eliom_lib
open Eliom_parameter

let bus = Eliom_bus.create [%derive.json: messages]
  
module Nestor_app =
  Eliom_registration.App (
    struct
      let application_name = "nestor"
    end)
    
let ro = ref None
let alive = ref false
let synchronized = ref false
let isActive = ref false
let lcd = ref stdout
let silentconn = ref false
let isDrivingForward = ref false
  
let close () =
   match !ro with
      None -> ()
    | Some cro -> (
      if !synchronized then begin
	Interface_local.stop_sync cro;
	synchronized := false;
      end;
      isActive := false;
      Interface_local.close_roomba cro;
      ro := None;
      ignore @@ Unix.close_process_out !lcd;
      silentconn := false;
      isDrivingForward := false;
    )
       
    
let rec sleep_thread () =
  let%lwt _ = Lwt_unix.sleep 10.0 in
  let%lwt _ = (if not !silentconn then Lwt_unix.sleep 120.0 else Lwt.return ()) in
  let%lwt _ = (if !synchronized then Lwt_unix.sleep 1200.0 else Lwt.return ()) in
  (match !ro with
    None -> ()
  | Some cro when not !alive -> close ();      
  | _ -> alive := false);
  sleep_thread ()

let slth = Lwt.async sleep_thread 
    
let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()
   
let get_uptime =
  Lwt_process.pread_line ("/usr/bin/uptime",[||])

let get_state () =
  match !ro with
    None -> Lwt.return Disconnected
  | Some _ when !synchronized -> Lwt.return @@ Synchronized !isActive
  | _ -> Lwt.return @@ Connected !isActive
     
     
let action_handling action =
  alive := true;
  let time = ref 0.0 in
  begin match !ro with
  | None -> 
     begin if action= Wakeup then begin
       Interface_local.wake_up ();
       try
	 ro := Some (Unix.handle_unix_error Interface_local.init_roomba "/dev/ttyAMA0");
	 isActive := false;
	 if not !silentconn then (
	   lcd := Unix.open_process_out "/usr/bin/python char_text.py";
	   output_string !lcd "Connected\n";
	   flush !lcd
	 )
       with _ -> Printf.fprintf stderr "fail to open Roomba"
     end;
     end;
  | Some cro -> 
     
     let open Type_def in
     let open Distance in
     begin match action with
     | Refresh | Wakeup ->
	if not !synchronized then (
	  Interface_local.query_list cro (*[1;2;3;4;5;101]*) [100];
          callbackfun ~cb:(fun x y r rs ->
	    let sl = print_list rs in
	    ignore @@ Eliom_bus.write bus (x,y,r,sl)
	  ) static_pt (Interface_local.get_state cro)
	);
	  
     | Synchronize -> if not !synchronized then begin
       (try 
	 Interface_local.sync_state cro [1;2;3;35;43;44;45;106]; (*[100]*)
       with
	 Type_def.Upstream_in_use -> ()
       );
       Interface_local.change_callback cro (
	 callbackfun
	   ~cb:(fun x y r rs ->
	     if (rs.bumpsWheeldrops |>>| 0) > 0
	       && !isDrivingForward then (
	       Interface_local.roomba_cmd cro (Drive (0,0));
		 isDrivingForward := false;
	       );
	     isActive := rs.oiMode |>>> (fun i -> i>=2) |>>| false;
	     let time2 = Unix.gettimeofday () in		
	     if time2-. !time > 0.15 then begin
	       time := time2;
	       let sl = print_list rs in
	       ignore @@ Eliom_bus.write bus (x,y,r,sl)
	     end) static_pt);
       output_string !lcd "Synchronized\n";
       flush !lcd;
       synchronized := true
     end
     | Stop_syn -> Interface_local.stop_sync cro;
       synchronized := false;
       output_string !lcd "Connected\n";
       flush !lcd
     | Safe -> Interface_local.roomba_cmd cro Safe;
       isActive := true;
	
     | Close -> close ()
	 
     | Power -> Interface_local.roomba_cmd cro Power
     | Max -> Interface_local.roomba_cmd cro Max;
       isActive := false;
       isDrivingForward := false;
	
     | Spot -> Interface_local.roomba_cmd cro Spot;
       isActive := false;
       isDrivingForward := false;
       
     | Clean -> Interface_local.roomba_cmd cro Clean;
       isActive := false;
       isDrivingForward := false;
       
     | Dock -> Interface_local.roomba_cmd cro Dock;
       isActive := false;
       isDrivingForward := false;
	
     | Move(x,y) -> Interface_local.roomba_cmd cro (Drive (x,y));
       isDrivingForward :=  x > 0 && ( y <> -1 || y <> 1) 
     end;
  end;
  Lwt.return unit

let csv_print label x l =
  match x with
    Some y -> (label,"\""^y^"\"")::l
  | None -> (label,"")::l
    
    
let get_state_action () =
  let rec get_ro () = match !ro with
      None ->
	silentconn := true;
	let%lwt _ = action_handling Wakeup in
	get_ro ()
    | Some r -> Lwt.return r in
  let%lwt cro = get_ro () in
  if not !synchronized then
    Interface_local.query_list cro (*[1;2;3;4;5;101]*) [3];
  let rs = Interface_local.get_state cro in
  let time = string_of_float @@ Unix.gettimeofday () in
  let sl = List.fold_left
    (*(fun c (n,v) -> Printf.sprintf " \"%s\":\"%s\",%s" n v c) ("\"time\":"^time)*)
    (fun c (_,v) -> Printf.sprintf "%s,%s" v c) time 
    (Type_def.print_list ~prfun:csv_print rs) in
  Lwt.return (sl ^ "\n")

let _ =
  Eliom_registration.Any.register_service
    ~path:["rest"]
    ~get_params: Eliom_parameter.unit
    (fun () () ->
      let%lwt answer = get_state_action () in
      Eliom_registration.String.send ~code:200
        (answer,"application/javascript") )
