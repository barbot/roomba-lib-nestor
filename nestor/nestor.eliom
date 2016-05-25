[%%shared
  (* Modules opened in the shared-section are available in client-
     and server-code *)
  open Eliom_content.Html5.D
  open Lwt


  type messages =
    float*float*float
      [@@deriving json]

      
]

let bus = Eliom_bus.create [%derive.json: messages]

  open Eliom_lib
  open Eliom_content
  open Eliom_parameter
  
module Nestor_app =
  Eliom_registration.App (
    struct
      let application_name = "nestor"
    end)

    
let%shared width = 700
let%shared height = 400

let%client draw ctx ((r, g, b), size, (x1, y1), (x2, y2)) =
  let color = CSS.Color.string_of_t (CSS.Color.rgb r g b) in
  ctx##.strokeStyle := (Js.string color);
  ctx##.lineWidth := float size;
  ctx##beginPath;
  ctx##(moveTo (float x1) (float y1));
  ctx##(lineTo (float x2) (float y2));
  ctx##stroke

let canvas_elt =
  canvas ~a:[a_width width; a_height height]
    [pcdata "your browser doesn't support canvas"]
    
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

let action_service_ro =
  div [table [
  tr [ td [ ];
       td [a (List.assoc "avance" action_services) [pcdata "^"] ()];
       td [] ;
       td []; td [a (List.assoc "spot" action_services) [pcdata "spot"] ()];
     ];
    tr [
      td [a (List.assoc "gauche" action_services) [pcdata "<"] ()];
      td [a (List.assoc "stop" action_services) [pcdata "o"] ()];
      td [a (List.assoc "droite" action_services) [pcdata ">"] ()];
      td []; td [a (List.assoc "clean" action_services) [pcdata "clean"] ()];
    ];
    tr [ td [ ];
	 td [a (List.assoc "recule" action_services) [pcdata "v"] ()];
	 td []; td []; td [a (List.assoc "dock" action_services) [pcdata "dock"] ()];
       ];
  ] 
      ]
    
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
  let lcoord = List.map (fun x ->  (200.0+.0.1*.x.Distance.posx, 200.0-.0.1*.x.Distance.posy)) tr in
  
  svg ~a:[
    Eliom_content.Svg.F.a_viewbox (0.0, 0.0, 400.0, 400.0);
    Eliom_content.Svg.F.a_height (400.0,Some `Px);
    Eliom_content.Svg.F.a_width (400.0,Some `Px);
  ]  
    [ Eliom_content.Svg.F.polyline ~a:[
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
	 let xc = ref 0.0 and yc = ref 0.0 and rc = ref 0.0 in
	 change_callback cro (callbackfun
				~cb:(fun x y r ->
				  if (abs_float !xc-.x)
				    +. (abs_float !yc-.y)
				    +. (abs_float !rc-.r) > 1.0 then
				    (xc:=x; yc:=y; rc:=r;
				     ignore @@ Eliom_bus.write bus (x,y,r)) ) static_pt);
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
	     action_service_ro ;
	     div ~a:[a_class ["well"]] bc ;
	     div ~a:[a_class ["sensor"]] [ul sensorval] ;
	     canvas_elt
(*	     div ~a:[a_class ["image"]] [ (svg_of_traj !Distance.static_traj) ];
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
	     ];*)
           ]))

let%client init_client () =

  let canvas = Eliom_content.Html5.To_dom.of_canvas ~%canvas_elt in
  let ctx = canvas##(getContext (Dom_html._2d_)) in
  ctx##.lineCap := Js.string "round";
  draw ctx ((0, 0, 0), 5, (0, 0), (width, 0));
  draw ctx ((0, 0, 0), 5, (width, 0), (width, height));
  draw ctx ((0, 0, 0), 5, (width, height), (0, height));
  draw ctx ((0, 0, 0), 5, (0, height), (0, 0));

  let x = ref 0 and y = ref 0 in

  let set_coord ev =
    let x0, y0 = Dom_html.elementClientPosition canvas in
    x := ev##.clientX - x0; y := ev##.clientY - y0
  in

  let compute_line ev =
    let oldx = !x and oldy = !y in
    set_coord ev;
    ((0, 0, 0), 5, (oldx, oldy), (!x, !y))
  in

  let compute_line2 ctx (xf,yf,r) =
    let oldx = !x and oldy = !y in
    x:= width/2 + int_of_float (xf*.0.01);
    y:= height/2 + int_of_float (yf*.0.01);
    let line = ((0, 0, 0), 5, (oldx, oldy), (!x, !y)) in
    draw ctx line
  in

  let line ev =
    let v = compute_line ev in
    draw ctx v;
    Lwt.return () in
  
  Lwt.async (fun () ->
    let open Lwt_js_events in
    mousedowns canvas
      (fun ev _ ->
         set_coord ev; line ev >>= fun () ->
           Lwt.pick
             [mousemoves Dom_html.document (fun x _ -> line x);
	      mouseup Dom_html.document >>= line]));
    
  Lwt.async (fun () -> Lwt_stream.iter (compute_line2 ctx) (Eliom_bus.stream ~%bus))
  
let () =
  Nestor_app.register
    ~service:main_service
    (fun () () ->
      let _ = [%client (init_client () : unit) ] in
      skeletton [p [pcdata "A, B, C..."]] "/");
  Nestor_app.register ~service:wakeup_service (fun () () ->
    let _ = [%client (init_client () : unit) ] in
    skeletton [p [pcdata "Waking up!"]] "wakeup");
  List.iter (fun (n,s) ->
    Nestor_app.register
      ~service:s
      (fun () () ->
	let _ = [%client (init_client () : unit) ] in
	skeletton [p [pcdata n]] n))
    action_services


