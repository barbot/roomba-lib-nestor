[%%shared
  (* Modules opened in the shared-section are available in client-
     and server-code *)
  open Eliom_content.Html5.D
  open Lwt


  type messages =
    float*float*float *
      ( (string*string) list)
      [@@deriving json]

  type order =
      Wakeup | Refresh | Close | Synchronize
    | Safe | Move of int*int
    | Clean | Power | Spot | Max | Dock
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
     begin if action= Wakeup then
	 Interface_local.wake_up ();
       ro := Some (Unix.handle_unix_error Interface_local.init_roomba "/dev/ttyAMA0");
     end;
  | Some cro -> 
     
     let open Type_def in
     let open Distance in
     begin match action with
     | Refresh | Wakeup ->
	if not !synchronized then
	  Interface_local.query_list cro [1;2;3;4;5;43;44;45;106;107];
        callbackfun ~cb:(fun x y r rs ->
	 xc:=x; yc:=y; rc:=r;
	 let sl = print_list rs in
	 ignore @@ Eliom_bus.write bus (x,y,r,sl)
       ) static_pt ( Interface_local.get_state cro);
       
     | Synchronize -> if not !synchronized then begin
       Interface_local.sync_state cro [1;2;3;43;44;45;106];
       Interface_local.change_callback cro (callbackfun
			      ~cb:(fun x y r rs ->
				if (abs_float (!xc-.x))
				  +. (abs_float (!yc-.y))
				  +. (abs_float 70.0*.(!rc-.r)) > 10.0 then begin
				    xc:=x; yc:=y; rc:=r;
				    let sl = print_list rs in
				    ignore @@ Eliom_bus.write bus (x,y,r,sl)
				    end) static_pt);
       synchronized := true
     end
     | Safe -> Interface_local.roomba_cmd cro Safe
     | Close -> Interface_local.roomba_cmd cro (Drive (0,0));
       Interface_local.close_roomba cro;
       ro := None
	 
     | Power -> Interface_local.roomba_cmd cro Power
     | Max -> Interface_local.roomba_cmd cro Max
	
     | Spot -> Interface_local.roomba_cmd cro Spot
     | Clean -> Interface_local.roomba_cmd cro Clean
     | Dock -> Interface_local.roomba_cmd cro Dock
	
     | Move(x,y) -> Interface_local.roomba_cmd cro (Drive (x,y))
     end;
  end;
  Lwt.return unit

let%client action_handling_client = ~%(server_function [%derive.json: order] action_handling)

let action_button x y =
  let onclick_handler = [%client (fun _ ->
    ignore @@ action_handling_client ~%x)] in
  button ~a:[a_onclick onclick_handler] [pcdata y;]

(*  
let action_service_button =
    List.map (fun x ->action_button x x) actions*)

let action_service_ro =
  div [table [
  tr [ td [];
       td [action_button (Move(100,0)) "^"];
       td [] ;
       td []; td [action_button Spot "spot"];
       td [action_button Safe "safe"];
     ];
    tr [
      td [action_button (Move(100,1)) "<"];
      td [action_button (Move(0,0)) "o"];
      td [action_button (Move(100,-1)) ">"];
      td []; td [action_button Clean "clean"];
      td [action_button Close "close"];
    ];
    tr [ td [ ];
	 td [action_button (Move(-100,0)) "v"];
	 td []; td []; td [action_button Dock "dock"];
	 td [action_button Synchronize "synchronize"];
       ];
    tr [
      td [ ];
      td [ ];
      td [ ];
            td [ ];
      td [action_button Refresh "refresh"];
      td [action_button Wakeup "wakeup"];
       ];
  ] 
      ]

    
let%shared width = 700
let%shared height = 400
    
let canvas_elt =
  canvas ~a:[a_width width; a_height height; a_class ["canvas"]]
    [pcdata "your browser doesn't support canvas"]
let sensor_div =
  div ~a:[ a_class ["sensorlistdiv"] ] [ul ~a:[a_id "sensorlist"] []]
    
let skeletton () =
  Lwt.return
        (Eliom_tools.F.html
           ~title:"Nestor"
           ~css:[["css";"nestor.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Nestor !"];
	     (*div  actionlist ;*)
	     (*(a wakeup_service [ pcdata "WakeUp"; br () ] ());*)
	     (*div ~a:[a_class ["action"]] action_service_button;*)
	     action_service_ro ;
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

[%%client    

 let xorg = ref (width/2)
 let yorg = ref (height/2)
 let scale = ref (0.05)

 let x_of_pt (x,_) =
   (float (!xorg)) +. !scale *. (float x)
 let y_of_pt (_,y) =
   (float (!yorg)) -. !scale *. (float y)
   
let draw ctx ((r, g, b), size, pt1, pt2) =
  let color = CSS.Color.string_of_t (CSS.Color.rgb r g b) in
  ctx##.strokeStyle := (Js.string color);
  ctx##.lineWidth := float size;
  ctx##beginPath;
  ctx##(moveTo (x_of_pt pt1) (y_of_pt pt1));
  ctx##(lineTo (x_of_pt pt2) (y_of_pt pt2));
  ctx##stroke

let draw_roomba ctx (r,g,b) pt rho =
  let color = CSS.Color.string_of_t (CSS.Color.rgb r g b) in
  ctx##.strokeStyle := (Js.string color);
  let color2 = CSS.Color.string_of_t (CSS.Color.rgb 0 0 0) in
  ctx##.lineWidth := float 2;
  ctx##beginPath;
  ctx##(arc (x_of_pt pt) (y_of_pt pt) (!scale *. 170.) 0.0 6.28318530717958 (Js.bool true));
  ctx##fill;
  ctx##.strokeStyle := (Js.string color2);
  ctx##beginPath;
  ctx##(moveTo (x_of_pt pt) (y_of_pt pt));
  ctx##(lineTo ( !scale *. 170.0 *. (cos rho) +. x_of_pt pt)
	  (!scale *. 170.0 *. (-.sin rho) +. y_of_pt pt));
  ctx##stroke
    
    
let init_client () =
  
  let canvas = Eliom_content.Html5.To_dom.of_canvas ~%canvas_elt in
  let sensors = Eliom_content.Html5.To_dom.of_div ~%sensor_div in
  let ctx = canvas##(getContext (Dom_html._2d_)) in
  (*ctx##.lineCap := Js.string "round";
  draw ctx ((0, 0, 0), 5, (0, 0), (width, 0));
  draw ctx ((0, 0, 0), 5, (width, 0), (width, height));
  draw ctx ((0, 0, 0), 5, (width, height), (0, height));
    draw ctx ((0, 0, 0), 5, (0, height), (0, 0));*)

  let x = ref (width/2) and y = ref (height/2) in

  (*let set_coord ev =
    let x0, y0 = Dom_html.elementClientPosition canvas in
    x := ev##.clientX - x0 + !xorg; y := - ev##.clientY - y0 + !yorg
  in

  let compute_line ev =
    let oldx = !x and oldy = !y in
    set_coord ev;
    ((0, 0, 0), 5, (oldx, oldy), (!x, !y))
    in*)

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
    x:= int_of_float xf;
    y:= int_of_float yf;
    let line = ((0, 0, 0), 1, (oldx, oldy), (!x, !y)) in
    draw ctx line;
    draw_roomba ctx (70, 70, 70) (!x, !y) r;
    let slHTML = ul ~a:[a_id "sensorlist"] (html_of_data sl) in
    let d = Dom_html.document in
    Dom.removeChild sensors (Js.Opt.get (d##getElementById (Js.string "sensorlist"))
			       (fun () -> assert false));
    Dom.appendChild
      sensors
      (Eliom_content.Html5.To_dom.of_ul slHTML)
  in

  let handle_msg ctx msg =
    compute_line2 ctx msg
  in
  
(*  let line ev =
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
    mouseup Dom_html.document >>= line]));*)
    
  Lwt.async (fun () -> Lwt_stream.iter (handle_msg ctx) (Eliom_bus.stream ~%bus));

  ignore @@ action_handling_client Refresh

]
    
let () =
  Nestor_app.register
    ~service:main_service
    (fun () () ->
      let _ = [%client (init_client () : unit) ] in
      skeletton ())
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
