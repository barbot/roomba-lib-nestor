[%%shared
  (* Modules opened in the shared-section are available in client-
     and server-code *)
  open Eliom_content.Html5.D
  open Lwt


  type messages =
    float*float*float *
      ( (string*string) list)
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

let actions = [ "wakeup"; "refresh"; "close"; "clean"; "power"; "spot"; "max" ; "dock";
		"safe"; "stop"; "avance"; "recule"; "droite"; "gauche";
		"synchronize"]

  (*
let wakeup_service =
  Eliom_service.App.service  ~get_params:Eliom_parameter.unit ~path:["wakeup"] ()

let action_service =
  Eliom_service.App.service ~get_params:(string "action") ~path:[] ()
  *)    

(*let action_services =
  List.map (fun x ->
  (x,Eliom_service.App.service  ~get_params:Eliom_parameter.unit ~path:[] ())) actions*)
    
(*let actions_service_link = 
  List.map (fun x ->
  a action_service [ pcdata x; br () ] x) actions*)

   
let get_uptime =
  Lwt_process.pread_line ("/usr/bin/uptime",[||])

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
    
let action_handling action =
  alive := true;
  let xc = ref 0.0 and yc = ref 0.0 and rc = ref 0.0 in
  begin match !ro with
  | None -> 
     begin if action="wakeup" then
	 Interface_local.wake_up ();
       ro := Some (Unix.handle_unix_error Interface_local.init_roomba "/dev/ttyAMA0");
     end;
  | Some cro -> 
     
     let open Type_def in
     let open Interface_local in
     let open Distance in
     begin match action with
     | "/" | "refresh" | "wakeup" ->
	if not !synchronized then query_list cro [1;2;3;4;5;43;44;45;106;107];
       callbackfun ~cb:(fun x y r rs ->
	 xc:=x; yc:=y; rc:=r;
	 let sl = print_list rs in
	 ignore @@ Eliom_bus.write bus (x,y,r,sl)
       ) static_pt (get_state cro);
       
     | "synchronize" -> if not !synchronized then begin
       sync_state cro [1;2;3;43;44;45;106];
       change_callback cro (callbackfun
			      ~cb:(fun x y r rs ->
				if (abs_float (!xc-.x))
				  +. (abs_float (!yc-.y))
				  +. (abs_float (!rc-.r)) > 10.0 then begin
				    xc:=x; yc:=y; rc:=r;
				    let sl = print_list rs in
				    ignore @@ Eliom_bus.write bus (x,y,r,sl)
				    end) static_pt);
       synchronized := true
     end
     | "safe" -> roomba_cmd cro Safe
     | "close" -> roomba_cmd cro (Drive (0,0));
       close_roomba cro;
       ro := None

	 
     | "power" -> roomba_cmd cro Power
     | "max" -> roomba_cmd cro Max
	
     | "spot" -> roomba_cmd cro Spot
     | "clean" -> roomba_cmd cro Clean
     | "dock" -> roomba_cmd cro Dock
	
     | "avance" -> roomba_cmd cro (Drive (100,0))
     | "recule" -> roomba_cmd cro (Drive (-100,0))
     | "droite" -> roomba_cmd cro (Drive (100,-1))
     | "gauche" -> roomba_cmd cro (Drive (100,1))
     | "stop" -> roomba_cmd cro (Drive (0,0))
     end;
  end;
  Lwt.return unit

let%client action_handling_client = ~%(server_function [%derive.json: string] action_handling)

let action_button x y =
  let onclick_handler = [%client (fun _ ->
    ignore @@ action_handling_client ~%x)] in
  button ~a:[a_onclick onclick_handler] [pcdata y;]

  
let action_service_button =
  List.map (fun x ->action_button x x) actions

let action_service_ro =
  div [table [
  tr [ td [];
       td [action_button "avance" "^"];
       td [] ;
       td []; td [action_button "spot" "spot"];
       td [action_button "safe" "safe"];
     ];
    tr [
      td [action_button "gauche" "<"];
      td [action_button "stop" "o"];
      td [action_button "droite" ">"];
      td []; td [action_button "clean" "clean"];
      td [action_button "close" "close"];
    ];
    tr [ td [ ];
	 td [action_button "recule" "v"];
	 td []; td []; td [action_button "dock"  "dock"];
	 td [action_button "synchronize" "synchronize"];
       ];
    tr [
      td [action_button "refresh" "refresh"];
      td [action_button "wakeup" "wakeup"];
       ];
  ] 
      ]

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
let sensor_div =
  div [ul ~a:[a_id "sensorlist"] []]
    
let skeletton bc action =
  (*let sensorval =
    alive := true;
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
       let open Distance in
  (*let ro = Unix.handle_unix_error init_roomba "/dev/ttyAMA0" in*)
  (*roomba_cmd ro WakeUp;*)
       begin match action with
       | "/" | "refresh" | "wakeup" -> ()
       | "synchronize" -> if not !synchronized then begin
	 sync_state cro [1;2;3;43;44;45;106];
	 let xc = ref 0.0 and yc = ref 0.0 and rc = ref 0.0 in
	 change_callback cro (callbackfun
				~cb:(fun x y r rs->
				  if (abs_float (!xc-.x))
				    +. (abs_float (!yc-.y))
				    +. (abs_float (!rc-.r)) > 10.0 then
				    (xc:=x; yc:=y; rc:=r;
				     ignore @@ Eliom_bus.write bus (x,y,r,print_list rs)) ) static_pt);
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
       (html_of_data cro) 
    end in*)
    
  (*close_roomba ro;*)
  
  Lwt.return
        (Eliom_tools.F.html
           ~title:"Nestor"
           ~css:[["css";"nestor.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Nestor !"];
	     (*div  actionlist ;*)
	     (*(a wakeup_service [ pcdata "WakeUp"; br () ] ());*)
	     div ~a:[a_class ["action"]] action_service_button;
	     action_service_ro ;
	     div ~a:[a_class ["well"]] bc ;
	     canvas_elt ;
	     sensor_div ;
	   (* div ~a:[a_class ["sensor"]] [ul sensorval] ;*)
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
  let xorg = ref 0 in
  let yorg = ref 0 in
  
  let canvas = Eliom_content.Html5.To_dom.of_canvas ~%canvas_elt in
  let sensors = Eliom_content.Html5.To_dom.of_div ~%sensor_div in
  let ctx = canvas##(getContext (Dom_html._2d_)) in
  ctx##.lineCap := Js.string "round";
  draw ctx ((0, 0, 0), 5, (0, 0), (width, 0));
  draw ctx ((0, 0, 0), 5, (width, 0), (width, height));
  draw ctx ((0, 0, 0), 5, (width, height), (0, height));
  draw ctx ((0, 0, 0), 5, (0, height), (0, 0));

  let x = ref (width/2) and y = ref (height/2) in

  let set_coord ev =
    let x0, y0 = Dom_html.elementClientPosition canvas in
    x := ev##.clientX - x0 + !xorg; y := - ev##.clientY - y0 + !yorg
  in

  let compute_line ev =
    let oldx = !x and oldy = !y in
    set_coord ev;
    ((0, 0, 0), 5, (oldx, oldy), (!x, !y))
  in

  (*let replace_child p n =
    Js.Opt.iter (p##firstChild) (fun c -> Dom.removeChild p c);
    Dom.appendChild p n
    in*)
  
  let html_of_data rl =
    List.map (fun (n,v) ->
      li [pcdata n ; pcdata ": "; pcdata v]) (*(
      ("posx", Printf.sprintf "%fmm" (!Distance.static_pt).posx )::
	("posy", Printf.sprintf "%fmm" (!Distance.static_pt).posy )::
					       ("angle", Printf.sprintf "%frad" (!Distance.static_pt).angle )::*)
	rl
  in
  
  let compute_line2 ctx (xf,yf,r,sl) =
    let oldx = !x and oldy = !y in
    x:= width/2 + int_of_float (xf*.0.1);
    y:= height/2 + int_of_float (yf*.0.1);
    let line = ((0, 0, 0), 1, (oldx, oldy), (!x, !y)) in
    draw ctx line;
    let slHTML = ul ~a:[a_id "sensorlist"] (html_of_data sl) in
    let d = Dom_html.document in
    Dom.removeChild sensors (Js.Opt.get (d##getElementById (Js.string "sensorlist"))
			       (fun () -> assert false));
    Dom.appendChild
      sensors
      (Eliom_content.Html5.To_dom.of_ul slHTML)
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
      skeletton [p [pcdata "A, B, C..."]] "/")
  (*Nestor_app.register ~service:wakeup_service (fun () () ->
    let _ = [%client (init_client () : unit) ] in
    skeletton [p [pcdata "Waking up!"]] "wakeup");*)
    (*List.iter (fun (n,s) ->*)
  (*Nestor_app.register
    ~service:action_service
    (fun action () ->
      let _ = [%client (init_client () : unit) ] in
    skeletton [p [pcdata action]] action)*)
(*) action_services*)
