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
let alive = ref false
let synchronized = ref false
  
let rec sleep_thread () =
  while true do 
    Unix.sleep 120;
    match !ro with
      None -> ()
    | Some cro when not !alive -> (
      Interface_local.roomba_cmd cro (Type_def.Drive (0,0));
      if !synchronized then begin
	Interface_local.stop_sync cro;
	synchronized := false;
      end;
      Interface_local.close_roomba cro;
      ro := None;)
    | _ -> alive := false
  done

let slth = Lwt_preemptive.detach sleep_thread ()
    
let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let actions = [ "refresh"; "close"; "clean"; "power"; "spot"; "max" ; "dock";
		"safe"; "stop"; "avance"; "recule"; "droite"; "gauche";
		"synchronize"]

let wakeup_service =
  Eliom_service.App.service  ~get_params:unit ~path:["wakeup"] ()
    
let action_services =
  List.map (fun x ->
    (x,Eliom_service.App.service  ~get_params:unit ~path:[x] ())) actions
    
let actions_service_link = 
  List.map (fun (x,y) ->
    a y [ pcdata x; br () ]  ()) action_services

let get_uptime =
  Lwt_process.pread_line ("/usr/bin/uptime",[||])
	   
let html_of_data r =
  List.map (fun (n,v) ->
    li [pcdata n ; pcdata ": "; pcdata v]  ) (
    ("posx", Printf.sprintf "%fmm" (!Distance.static_pt).posx )::
      ("posy", Printf.sprintf "%fmm" (!Distance.static_pt).posy )::
      ("angle", Printf.sprintf "%frad" (!Distance.static_pt).angle )::
      (Type_def.print_list (Interface_local.get_state r)))

let svg_of_traj tr =
 (* let coords = List.fold_left
    (fun x y -> Printf.sprintf "%s %f %f" x y.Distance.posx y.Distance.posy) "" tr in
  let img = "<svg viewBox = \"0 0 200 200\" version = \"1.1\">
    <polyline points = \""^coords^"\" fill = \"none\" stroke = \"black\" stroke-width = \"3\"/>
    </svg>" in*)
  let lcoord = List.map (fun x ->  (x.Distance.posx, x.Distance.posy)) tr in
  
  svg ~a:[Eliom_content.Svg.F.a_viewbox (-200.0, -200.0, 200.0, 200.0)] [
    Eliom_content.Svg.F.polyline ~a:[
      Eliom_content.Svg.F.a_points lcoord;
      Eliom_content.Svg.F.a_style "fill:none;stroke:black;stroke-width:3"
    ] []
  ]

    
let skeletton bc action =
  let sensorval,actionlist =
    alive := true;
    begin match !ro with
    | None ->
       begin if action="wakeup" then
	 Interface_local.wake_up ();
	 ro := Some (Unix.handle_unix_error Interface_local.init_roomba "/dev/ttyAMA0");
       end;
      [], [(a wakeup_service [ pcdata "WakeUp"; br () ] ())]
    | Some cro -> 
       
       let open Type_def in
       let open Interface_local in
       let open Distance in
  (*let ro = Unix.handle_unix_error init_roomba "/dev/ttyAMA0" in*)
  (*roomba_cmd ro WakeUp;*)
       begin match action with
       | "/" | "refresh" | "wakeup" -> ()
       | "synchronize" -> if not !synchronized then begin
	 sync_state cro [1;2;3;43;44;45;106];
	 change_callback cro (callbackfun static_pt);
	 synchronized := true
       end
       | "safe" -> roomba_cmd cro Safe
       | "close" -> roomba_cmd cro (Drive (0,0));
		    close_roomba cro;
		    ro := None
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
       if not !synchronized then query_list cro [1;2;3;4;5;43;44;45;106;107];
       (html_of_data cro),(a wakeup_service [ pcdata "WakeUp"; br () ] ())::actions_service_link 
    end in
    
  (*close_roomba ro;*)
  
  Lwt.return
        (Eliom_tools.F.html
           ~title:"Nestor"
           ~css:[["css";"nestor.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Nestor !"];
	     div ~a:[a_class ["action"]] actionlist ;
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
	     ];
	     div ~a:[a_class ["image"]] [ (svg_of_traj !Distance.static_traj) ]
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
