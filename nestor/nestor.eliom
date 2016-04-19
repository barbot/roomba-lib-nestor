{server{

  open Eliom_lib
  open Eliom_content
  open Html5.F
  open Eliom_parameter
  
module Nestor_app =
  Eliom_registration.App (
    struct
      let application_name = "nestor"
    end)
    
let ro = ref None

let rec sleep_thread () =
  while true do 
    Lwt.join [ Lwt_unix.sleep 10.0];
    match !ro with
      None -> ()
    | Some cro -> (
      Interface_local.close_roomba cro;
      ro := None;
    )
  done

let slth = Lwt_preemptive.detach sleep_thread ()
    
let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let actions = [ "refresh"; "clean"; "power"; "spot"; "max" ; "dock";
	      "safe"; "stop"; "avance"; "recule"; "droite"; "gauche"]

let wakeup_service =
  Eliom_service.App.service  ~get_params:unit ~path:["wakeup"] ()
    
let action_services =
  List.map (fun x ->
    (x,Eliom_service.App.service  ~get_params:unit ~path:[x] ())) actions
    
let actions_service_link =
  (a wakeup_service [ pcdata "WakeUp"; br () ] ())::
      List.map (fun (x,y) ->
	a y [ pcdata x; br () ]  ()) action_services
  

let html_of_data r =
  List.map (fun (n,v) ->
    li [pcdata n ; pcdata ": "; pcdata v]  ) (Type_def.print_list (Interface_local.get_state r))
    
let skeletton bc action =
  let sensorval =
    begin match !ro with
    | None ->
       begin if action="wakeup" then
	 Interface_local.wake_up ();
	 ro := Some (Unix.handle_unix_error Interface_local.init_roomba "/dev/ttyAMA0");
       end;
      []
    | Some cro -> 
       
       let open Type_def in
       let open Interface_local in
  (*let ro = Unix.handle_unix_error init_roomba "/dev/ttyAMA0" in*)
  (*roomba_cmd ro WakeUp;*)
       begin match action with
       | "/" | "refresh" -> ()
       | "safe" -> roomba_cmd cro Safe
       | "power" -> roomba_cmd cro Power
       | "spot" -> roomba_cmd cro Spot
       | "clean" -> roomba_cmd cro Clean
       | "max" -> roomba_cmd cro Max
       | "dock" -> roomba_cmd cro Dock
	  
       | "avance" -> roomba_cmd cro (Drive (100,0))
       | "recule" -> roomba_cmd cro (Drive (-100,0))
       | "droite" -> roomba_cmd cro (Drive (100,-1))
       | "gauche" -> roomba_cmd cro (Drive (100,1))
       | "stop" -> roomba_cmd cro (Drive (0,0))
       end;
       query_list cro [1;2;3;43;44;45;106];
       html_of_data cro 
    end in
    
  (*close_roomba ro;*)
  
  Lwt.return
        (Eliom_tools.F.html
           ~title:"Nestor"
           ~css:[["css";"nestor.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Nestor !"];
	     div ~a:[a_class ["action"]] actions_service_link ;
	     div ~a:[a_class ["well"]] bc ;
	     div ~a:[a_class ["sensor"]] [ul sensorval] ;
	     div ~a:[a_class ["image"]] [
		   img ~alt:("Ocsigen Logo")
		       ~src:(make_uri
			       ~service:(Eliom_service.static_dir ())
			       ["img/DSC_0983.jpg"])
		       () ;
		   img ~alt:("Ocsigen Logo")
		       ~src:(make_uri
			       ~service:(Eliom_service.static_dir ())
			       ["img/DSC_0984.jpg"])
		       () ;
		 ]
           ]))

    
let () =
  Nestor_app.register
    ~service:main_service
    (fun () () -> skeletton [p [pcdata "A, B, C..."]] "/");
  Nestor_app.register ~service:wakeup_service (fun () () ->
	skeletton [p [pcdata "Waking up!"]] "wakeup");
  List.iter (fun (n,s) ->
    Nestor_app.register
      ~service:s
      (fun () () ->
	skeletton [p [pcdata n]] n))
    action_services

    }}
