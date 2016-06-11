[%%shared
  (* Modules opened in the shared-section are available in client-
     and server-code *)
  open Eliom_content.Html5.D
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
let isActive = ref false
  
let rec sleep_thread () =
  while true do 
    Unix.sleep 120;
    match !ro with
      None -> ()
    | Some cro when not !alive -> (
      if !synchronized then begin
	Interface_local.stop_sync cro;
	synchronized := false;
      end;
      isActive := false;
      Interface_local.close_roomba cro;
      ro := None;)
    | _ -> alive := false
  done

let slth = Lwt_preemptive.detach sleep_thread ()
    
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
       with _ -> Printf.fprintf stderr "fail to open Roomba"
     end;
     end;
  | Some cro -> 
     
     let open Type_def in
     let open Distance in
     begin match action with
     | Refresh | Wakeup ->
	if not !synchronized then
	  Interface_local.query_list cro [1;2;3;4;5;43;44;45;106;107];
        callbackfun ~cb:(fun x y r rs ->
	 let sl = print_list rs in
	 ignore @@ Eliom_bus.write bus (x,y,r,sl)
       ) static_pt ( Interface_local.get_state cro);
       
     | Synchronize -> if not !synchronized then begin
       Interface_local.sync_state cro [1;2;3;43;44;45;106];
       Interface_local.change_callback cro (callbackfun
			        ~cb:(fun x y r rs ->
				  let time2 = Unix.gettimeofday () in		
				if time2-. !time > 0.15 then begin
				    time := time2;
				    let sl = print_list rs in
				    ignore @@ Eliom_bus.write bus (x,y,r,sl)
				    end) static_pt);
       synchronized := true
     end
     | Stop_syn -> Interface_local.stop_sync cro;
	synchronized := false;
     | Safe -> Interface_local.roomba_cmd cro Safe;
       isActive := true;
	
     | Close -> Interface_local.close_roomba cro;
       ro := None
	 
     | Power -> Interface_local.roomba_cmd cro Power
     | Max -> Interface_local.roomba_cmd cro Max;
       isActive := false;
	
     | Spot -> Interface_local.roomba_cmd cro Spot;
	isActive := false;
     | Clean -> Interface_local.roomba_cmd cro Clean;
       isActive := false;
     | Dock -> Interface_local.roomba_cmd cro Dock;
       isActive := false;
	
     | Move(x,y) -> Interface_local.roomba_cmd cro (Drive (x,y))
     end;
  end;
  Lwt.return unit

let%client action_handling_client = ~%(server_function [%derive.json: order] action_handling)
let%client get_state_client = ~%(server_function [%derive.json: unit] get_state)

let%shared width = 700
let%shared height = 400
    
let canvas_elt =
  canvas ~a:[a_width width; a_height height;]
    [pcdata "your browser doesn't support canvas"]
let sensor_div =
  div ~a:[ a_class ["sensorlistdiv"] ] [ul ~a:[a_id "sensorlist"] []]
let%shared button_div =
  div ~a:[ a_class ["buttondiv"]] [table ~a:[a_id "buttonid"] [] ]

[%%client

let rec action_button f x y =
  let onclick_handler = (fun _ ->
    ignore @@ action_handling_client x;ignore @@ f ()) in
  button ~a:[a_onclick onclick_handler] [pcdata y;]

(*  
let action_service_button =
    List.map (fun x ->action_button x x) actions*)
    
let action_service_ro f = function
  Disconnected -> table ~a:[a_id "buttonid"] [
    tr [ td [];td [];td [];td [];td [];td []; ];
    tr [ td [];td [];td [];td [];td [];td []; ];
    tr [ td [];td [];td [];td [];td [];td []; ];
    tr [ td [];td [];td [];td [];td [action_button f Refresh "refresh"];
	 td [action_button f Wakeup "wakeup"];];
  ] 
  | Connected true ->  
  table ~a:[a_id "buttonid"] [
    tr [ td [];td [action_button f (Move(100,0)) "^"];td [];
	 td [];td [action_button f Spot "spot"];
	 td [];];
    tr [
      td [action_button f (Move(100,1)) "<"];
      td [action_button f (Move(0,0)) "o"];
      td [action_button f (Move(100,-1)) ">"];
      td []; td [action_button f Clean "clean"];
      td [action_button f Close "close"];
    ];
    tr [ td [];td [action_button f (Move(-100,0)) "v"];
	 td []; td [];td [action_button f Dock "dock"];
	 td [action_button f Synchronize "synchronize"];
       ];
    tr [ td [];td [];td [];td [];td [action_button f Refresh "refresh"];td [];];
  ]
  | Connected false ->  
  table ~a:[a_id "buttonid"] [
    tr [ td [];td [];td [];td [];td [action_button f Spot "spot"];
	 td [action_button f Safe "safe"];];
    tr [
      td [];td [];td [];td []; td [action_button f Clean "clean"];
      td [action_button f Close "close"];
    ];
    tr [ td [];td [];td []; td [];td [action_button f Dock "dock"];
	 td [action_button f Synchronize "synchronize"];
       ];
    tr [ td [];td [];td [];td [];td [action_button f Refresh "refresh"];td [];];
  ]
  | Synchronized true ->  
  table ~a:[a_id "buttonid"] [
    tr [ td [];td [action_button f (Move(100,0)) "^"];td [];
	 td [];td [action_button f Spot "spot"];
	 td [];];
    tr [
      td [action_button f (Move(100,1)) "<"];
      td [action_button f (Move(0,0)) "o"];
      td [action_button f (Move(100,-1)) ">"];
      td []; td [action_button f Clean "clean"];
      td [action_button f Close "close"];
    ];
    tr [ td [];td [action_button f (Move(-100,0)) "v"];
	 td []; td [];td [action_button f Dock "dock"];
	 td [action_button f Stop_syn "Unsynchronize"];
       ];
    tr [ td [];td [];td [];td [];td [action_button f Refresh "refresh"];td [];];
  ]
  | Synchronized false ->
     table ~a:[a_id "buttonid"] [
    tr [ td [];td [];td [];td [];td [action_button f Spot "spot"];
	 td [action_button f Safe "safe"];];
    tr [
      td [];td [];td [];td []; td [action_button f Clean "clean"];
      td [action_button f Close "close"];
    ];
    tr [ td [];td [];td []; td [];td [action_button f Dock "dock"];
	 td [action_button f Stop_syn "Unsynchronize"];
       ];
    tr [ td [];td [];td [];td [];td [action_button f Refresh "refresh"];td [];];
     ]
       
    
 let xorg = ref (width/2)
 let yorg = ref (height/2)
 let scale = ref (0.05)

 let x_of_pt (x,_) =
   (float (!xorg)) +. !scale *. x
 let y_of_pt (_,y) =
   (float (!yorg)) -. !scale *. y

 let clean ctx =
   let color = CSS.Color.string_of_t (CSS.Color.rgb 200 200 200) in
   ctx##.fillStyle := (Js.string color);
   ctx##(fillRect 0.0 0.0 (float width) (float height))
     
let draw ctx ((r, g, b), size, pt1, pt2) =
  let color = CSS.Color.string_of_t (CSS.Color.rgb r g b) in
  ctx##.strokeStyle := (Js.string color);
  ctx##.lineWidth := float size;
  ctx##beginPath;
  ctx##(moveTo (x_of_pt pt1) (y_of_pt pt1));
  ctx##(lineTo (x_of_pt pt2) (y_of_pt pt2));
  ctx##stroke

let draw_roomba ctx (r,g,b) (x,y,rho) =
  let pt = (x,y) in
  let color = CSS.Color.string_of_t (CSS.Color.rgb r g b) in
  let color2 = CSS.Color.string_of_t (CSS.Color.rgb 0 0 0) in
  ctx##.fillStyle := (Js.string color);
  ctx##.strokeStyle := (Js.string color2);
  ctx##.lineWidth := float 2;
  ctx##beginPath;
  ctx##(arc (x_of_pt pt) (y_of_pt pt) (!scale *. 170.) 0.0 6.28318530717958 (Js.bool true));
  ctx##fill;
  ctx##stroke;
  ctx##beginPath;
  ctx##(moveTo (x_of_pt pt) (y_of_pt pt));
  ctx##(lineTo ( !scale *. 170.0 *. (cos rho) +. x_of_pt pt)
	  (!scale *. 170.0 *. (-.sin rho) +. y_of_pt pt));
  ctx##stroke

    
let compute_line ctx (xf,yf,r) (xf2,yf2,r2) =
  let line = ((0, 0, 0), 1, (xf, yf), (xf2, yf2)) in
  draw ctx line
  
let compute_line2 ctx l =
  begin match l with
    [] -> ()
  | t::q -> ignore @@ List.fold_left (fun pt1 pt2 ->
    compute_line ctx pt1 pt2;
    pt2) t q;
    draw_roomba ctx (150, 150, 150) t;
  end

  
let poslist = ref [0.0,0.0,0.0]
  
let draw_all ctx =
  clean ctx;
  compute_line2 ctx !poslist
let drawb () =
  let canvas = Eliom_content.Html5.To_dom.of_canvas ~%canvas_elt in
  draw_all (canvas##(getContext (Dom_html._2d_)))

let init_client () =
  
  let canvas = Eliom_content.Html5.To_dom.of_canvas ~%canvas_elt in
  let sensors = Eliom_content.Html5.To_dom.of_div ~%sensor_div in
  let buttons = Eliom_content.Html5.To_dom.of_div ~%button_div in
  let ctx = canvas##(getContext (Dom_html._2d_)) in
  (*ctx##.lineCap := Js.string "round";
  draw ctx ((0, 0, 0), 5, (0, 0), (width, 0));
  draw ctx ((0, 0, 0), 5, (width, 0), (width, height));
  draw ctx ((0, 0, 0), 5, (width, height), (0, height));
    draw ctx ((0, 0, 0), 5, (0, height), (0, 0));*)

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
  
  let handle_msg ctx (xf,yf,r,sl) =
    begin match !poslist with
      (x,y,rl)::_ when x <> xf || y <> xf || rl <> r ->
	poslist := (xf,yf,r)::(!poslist)
    | [] -> poslist := [(xf,yf,r)]
    | _ -> ()
    end;
    draw_all ctx;
    let slHTML = ul ~a:[a_id "sensorlist"] (html_of_data sl) in
    let d = Dom_html.document in
    Dom.removeChild sensors (Js.Opt.get (d##getElementById (Js.string "sensorlist"))
			       (fun () -> assert false));
    Dom.appendChild
      sensors
      (Eliom_content.Html5.To_dom.of_ul slHTML)
  in

  let rec update_state () =
    let%lwt state = get_state_client () in
    let tabHTML = action_service_ro update_state state in
    let d = Dom_html.document in
    Dom.removeChild buttons (Js.Opt.get (d##getElementById (Js.string "buttonid"))
			       (fun () -> assert false));
    Dom.appendChild
      buttons
      (Eliom_content.Html5.To_dom.of_table tabHTML);
    Lwt.return ()
  in
      
(*  let line ev =
    let v = compute_line ev in
    draw ctx v;
    Lwt.return () in*)

  let xmouse = ref 0 and ymouse = ref 0 in
  let translate ev =
    xorg := !xorg + ev##.clientX - !xmouse;
    yorg := !yorg + ev##.clientY - !ymouse;
    xmouse := ev##.clientX;
    ymouse := ev##.clientY;
    drawb ();
    Lwt.return ()
  in
    
  Lwt.async (fun () ->
    let open Lwt_js_events in
    mousedowns canvas
      (fun ev _ ->
	xmouse := ev##.clientX;
	ymouse := ev##.clientY;
        Lwt.pick
          [mousemoves Dom_html.document (fun x _ -> translate x);
	   mouseup Dom_html.document >>= translate]));
    
  Lwt.async (fun () -> Lwt_stream.iter (handle_msg ctx) (Eliom_bus.stream ~%bus));

  drawb ();
  ignore @@ update_state ();
  ignore @@ action_handling_client Refresh
]

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
	     button_div;
	     (div ~a:[a_class ["canvas"]] [
	       canvas_elt ; br ();
	       button ~a:[a_onclick [%client fun _ -> (scale := !scale /. 1.2; drawb ()) ]] [pcdata "-";];
	       button ~a:[a_onclick [%client fun _ -> (scale := 1.2*. !scale; drawb ()) ]] [pcdata "+";];
	     ]);
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
      
let () =
  Nestor_app.register
    ~service:main_service
    (fun () () ->
      let page = skeletton () in
      let _ = [%client (init_client () : unit) ] in
      page
      ) 
