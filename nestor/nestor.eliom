[%%shared
  (* Modules opened in the shared-section are available in client-
     and server-code *)
  open Eliom_content.Html5.D
  open Lwt
  open Server_side
]


open Eliom_content
    
let%client action_handling_client = ~%(server_function [%derive.json: order] action_handling)
let%client get_state_client = ~%(server_function [%derive.json: unit] get_state)

let%shared width = 700
let%shared height = 400
    
let canvas_elt =
  canvas ~a:[a_width width; a_height height;]
    [pcdata "your browser doesn't support canvas"]
let canvas_graph =
  canvas ~a:[a_width 700; a_height 250;]
     [pcdata "your browser doesn't support canvas"]
let sensor_div =
  div ~a:[ a_class ["sensorlistdiv"] ] [ul ~a:[a_id "sensorlist"] [li [pcdata "No data"]]]
let%shared button_div =
  div ~a:[ a_class ["buttondiv"]] [table ~a:[a_id "buttonid"] [] ]


let rec next_val p1 s =
  try
    let p2 = String.index_from s (p1+1) ',' in
    (String.sub s (p1+1) (p2-p1-1))::(next_val p2 s)
  with Not_found -> [String.sub s (p1+1) (String.length s -p1-1)]

let match_end s2 s1 =
  let n1 = String.length s1
  and n2 = String.length s2 in
  if n1 >= n2 then
    if (String.sub s1 (n1-n2) n2) = s2 then
      String.sub s1 0 (n1-n2)
    else s1
  else s1
    
let clean s =
  let s2 = String.trim s in
  if s2 <> "" then
    let s3 = (if String.get s2 0 = '"' && String.get s2 (String.length s2 -1) = '"' then
	String.sub s2 1 (String.length s2 -2) else s2) in
    s3
  |> match_end "mAh"
  |> match_end "mA"
  |> match_end "mV"
  |> match_end "°C"
  else ""

let select_line sl =
  try
    [ (float_of_string @@ List.nth sl 49);
      (float_of_string @@ List.nth sl 15);
      (float_of_string @@ List.nth sl 16);
      (float_of_string @@ List.nth sl 17);
      (float_of_string @@ List.nth sl 18);
       ]
      with _ -> []
  

let rec read_log_line f =
  try 
  let s = input_line f in
  let logl = List.map clean @@
    next_val 0 s in
  (select_line logl) :: read_log_line f
  with End_of_file -> []
   
let get_log () =
  let f = open_in "/home/pi/lognestor" in
  let l = read_log_line f in
  close_in f;
  l;;

let all_log = ref (get_log ())   
       
[%%client

 let speed_slider = Raw.input ~a:[a_id "speedid";
				  a_input_type `Range;
				  a_input_min 0.0;
				  a_input_max 500.0;
				  a_value "100"] ()
  
let rec action_button f x y =
  let onclick_handler = (fun _ ->
    let slide = Eliom_content.Html5.To_dom.of_input speed_slider in
    let speed = int_of_string @@ Js.to_string slide##.value in
    print_endline @@ string_of_int speed; 
    ignore @@ action_handling_client (x speed);ignore @@ f ()) in
  button ~a:[a_onclick onclick_handler] [pcdata y;]
    
(*  
let action_service_button =
    List.map (fun x ->action_button x x) actions*)
    
let action_service_ro f = function (*match Connected true with*)
  | Disconnected -> table ~a:[a_id "buttonid"] [
    tr [ td ~a:[a_colspan 6] [pcdata "Disconnected"]; ];
    tr [ td [];td [];td [];td [];td [];td []; ];
    tr [ td [];td [];td [];td [];td [];td []; ];
    tr [ td [];td [];td [];td [];td [];td []; ];
    tr [ td [];td [];td [];td [];td [action_button f (fun _ ->Refresh) "refresh"];
	 td [action_button f (fun _ ->Wakeup) "wakeup"];];
  ] 
  | Connected true -> table ~a:[a_id "buttonid"] [
    tr [ td ~a:[a_colspan 6] [pcdata "Connected Direct Control"]; ];
    tr [ td [action_button f (fun s ->Move(s,1000)) "\\"];
	 td [action_button f (fun s ->Move(s,0)) "^"];
	 td [action_button f (fun s ->Move(s,-1000)) "/"];
	 td [action_button f (fun _->Motor 7) "BrushOn"];
	 td [action_button f (fun _->Spot) "spot"];
	 td [];];
    tr [
      td [action_button f (fun s ->Move(s,1)) "<"];
      td [action_button f (fun _ ->Move(0,0)) "o"];
      td [action_button f (fun s ->Move(s,-1)) ">"];
      td [action_button f (fun _->Motor 0) "BrushOff"];
      td [action_button f (fun _ ->Clean) "clean"];
      td [action_button f (fun _ ->Close) "close"];
    ];
    tr [ td [];td [action_button f (fun s ->Move(-s,0)) "v"];
	 td [];td [action_button f (fun _->Explore) "Explore"];
	 td [action_button f (fun _->Dock) "dock"];
	 td [action_button f (fun _->Synchronize) "synchronize"];
       ];
    tr [ td ~a:[a_colspan 4] [ speed_slider ];
	 td [action_button f (fun _->Refresh) "refresh"];td [];];
  ]
  | Connected false -> table ~a:[a_id "buttonid"] [
    tr [ td ~a:[a_colspan 6] [pcdata "Connected"]; ];
    tr [ td [];td [];td [];td [];td [action_button f (fun _->Spot) "spot"];
	 td [action_button f (fun _->Safe) "safe"];];
    tr [
      td [];td [];td [];td []; td [action_button f (fun _->Clean) "clean"];
      td [action_button f (fun _->Close) "close"];
    ];
    tr [ td [];td [];td []; td [];td [action_button f (fun _->Dock) "dock"];
	 td [action_button f (fun _->Synchronize) "synchronize"];
       ];
    tr [ td [];td [];td [];td [];td [action_button f (fun _->Refresh) "refresh"];td [];];
  ]
  | Synchronized true -> table ~a:[a_id "buttonid"] [
    tr [ td ~a:[a_colspan 6] [pcdata "Synchronized Direct Control"]; ];
    tr [ td [action_button f (fun s ->Move(s,1000)) "\\"];
	 td [action_button f (fun s ->Move(s,0)) "^"];
	 td [action_button f (fun s ->Move(s,-1000)) "/"];
	 td [action_button f (fun _->Motor 7) "BrushOn"];
	 td [action_button f (fun _->Spot) "spot"];
	 td [];];
    tr [
      td [action_button f (fun s ->Move(s,1)) "<"];
      td [action_button f (fun _ ->Move(0,0)) "o"];
      td [action_button f (fun s ->Move(s,-1)) ">"];
      td [action_button f (fun _->Motor 0) "BrushOff"];
      td [action_button f (fun _ ->Clean) "clean"];
      td [action_button f (fun _ ->Close) "close"];
    ];
    tr [ td [];td [action_button f (fun s->Move(-s,0)) "v"];
	 td [];td [action_button f (fun _->Explore) "Explore"];
	 td [action_button f (fun _->Dock) "dock"];
	 td [action_button f (fun _->Stop_syn) "Unsynchronize"];
       ];
    tr [ td ~a:[a_colspan 4] [ speed_slider ]; td [action_button f (fun _->Refresh) "refresh"];td [];];
  ]
  | Synchronized false -> table ~a:[a_id "buttonid"] [
    tr [ td ~a:[a_colspan 6] [pcdata "Synchronized"]; ];
    tr [ td [];td [];td [];td [];td [action_button f (fun _->Spot) "spot"];
	 td [action_button f (fun _->Safe) "safe"];];
    tr [
      td [];td [];td [];td []; td [action_button f (fun _->Clean) "clean"];
      td [action_button f (fun _->Close) "close"];
    ];
    tr [ td [];td [];td []; td [];td [action_button f (fun _->Dock) "dock"];
	 td [action_button f (fun _->Stop_syn) "Unsynchronize"];
       ];
    tr [ td [];td [];td [];td [];td [action_button f (fun _->Refresh) "refresh"];td [];];
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

let bounding_box g =
  List.fold_left (fun (xmin,xmax,ymin,ymax) (x,y)->
    (min xmin x),(max xmax x),(min ymin y),(max ymax y))
    (max_float,min_float,max_float,min_float)
    g

let normalize bb w h g =
  let xmin,xmax,ymin,ymax = bb in
  List.map (fun (x,y) ->
     ((x-.xmin) /. (xmax-.xmin) *. float w),
     ((ymax-.y) /. (ymax-.ymin) *. float h)) g

let dlines ctx g =
  let dl (x1,y1) (x2,y2) = ctx##beginPath;
    ctx##(moveTo x1 y1);
    ctx##(lineTo x2 y2);
    ctx##stroke in
  ignore @@ List.fold_left (fun p1 p2 ->
    dl p1 p2;
    p2)
    (List.hd g) g
    
let draw_graph (r,v,b) canvas g =
  if List.length g >0 then begin 
    let ctx = canvas##(getContext (Dom_html._2d_)) in
    let bb = bounding_box g in
    let g2 = normalize bb 680 200 g in
    let color = CSS.Color.string_of_t (CSS.Color.rgb r v b) in
    ctx##.strokeStyle := (Js.string color);
    ctx##.lineWidth := 3.0;
    dlines ctx g2;
    let xmin,xmax,ymin,ymax = bb in
    if ymin*.ymax < 0.0 then begin
      ctx##.lineWidth := 1.0;
      let g3 = normalize bb 680 200 [xmin,0.0; xmax,0.0] in
      dlines ctx g3;
    end
  end

let log_charge () = List.map (fun x -> List.hd x , List.nth x 4) !(~%all_log)
let log_tmp () = List.map (fun x -> List.hd x , List.nth x 3) !(~%all_log)
let log_volt () = List.map (fun x -> List.hd x , List.nth x 1) !(~%all_log)
let log_conso () = List.map (fun x -> List.hd x , List.nth x 2) !(~%all_log)
let log_time () = let xmin,xmax,_,_ = bounding_box (log_charge ()) in
		  let nj = (xmax -.xmin) /. 60.0 in () 
    
let init_client () =
  let canvas = Eliom_content.Html5.To_dom.of_canvas ~%canvas_elt in
  let sensors = Eliom_content.Html5.To_dom.of_div ~%sensor_div in
  let buttons = Eliom_content.Html5.To_dom.of_div ~%button_div in
  let ctx = canvas##(getContext (Dom_html._2d_)) in
  let canvas_g =  Eliom_content.Html5.To_dom.of_canvas ~%canvas_graph in
  
  draw_graph (0,0,200) canvas_g (log_charge ());
  draw_graph  (200,0,0) canvas_g (log_tmp ());
  draw_graph  (0,0,0) canvas_g (log_volt ());
  draw_graph  (0,200,0) canvas_g (log_conso ());
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
      (x,y,rl)::_ when
	  (abs_float (x -. xf))
	  +. (abs_float (y -. yf))
	  +. (70.0*.(abs_float (r -. rl))) > 10.0 ->
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
  all_log := get_log ();   
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
	       button ~a:[a_onclick [%client fun _ -> (poslist := []; drawb ()) ]] [pcdata "clear";];
		 
		 
	     ]);
	     sensor_div ;
	   (* div ~a:[a_class ["sensor"]] [ul sensorval] ;*)
	     (*	     div ~a:[a_class ["image"]] [ (svg_of_traj !Distance.static_traj) ];*)
	     div ~a:[a_class ["image"]] [
		   img ~alt:("Ocsigen Logo")
		       ~src:(make_uri
			       ~service:(Eliom_service.static_dir ())
			       ["img/DSC_1040.jpg"])
		       () ;
		  (* img ~alt:("Ocsigen Logo")
		       ~src:(make_uri
			       ~service:(Eliom_service.static_dir ())
			       ["img/DSC_0984.jpg"])
		     () ;*)
	     ];
	     (div ~a:[a_class ["canvas"]] [canvas_graph;]);
           ]))
      
let () =
  Nestor_app.register
    ~service:main_service
    (fun () () ->
      let page = skeletton () in
      let _ = [%client (init_client () : unit) ] in
      page
    )
