open Type_def

let echelle = ref 0.005 
let xtrans = ref 0.0
let ytrans = ref 0.0


let posx = ref 0.0
let posy = ref 0.0
  
let time_draw = ref (Unix.gettimeofday ())
	
let jostickon = ref false
  

let angle = ref 0.0
let trajectory = ref [0.0, 0.0 , 0.0]
let data_string = ref "Data Not Available" 
  
let add_trajectory pt =
  match !trajectory with
      [] -> trajectory := [pt]
    | t::_ when pt = t -> ()
    | x -> trajectory := pt::x
      
let callbackfun str s =
  try
    let dist,dangle = Distance.movement s in
    angle := !angle +. dangle;
    posx := !posx +. ( dist *. (cos !angle ));
    posy := !posy +. ( dist *. (sin !angle ));
    add_trajectory (!posx, !posy, !angle); 
    data_string := Printf.sprintf "%s%sPos X: %f\nPos Y: %f" str
      (Type_def.string_of_available_data s) !posx !posy
  with
      Data_not_available -> ()
    | x -> print_endline "exception in the callback";
      raise x
	
let posxscreen = ref 0.0
and posyscreen = ref 0.0
  
let movescreen (xa,ya) =
  posxscreen := xa;
  posyscreen := ya
    
let fill_circle x y r=
  GlDraw.begins `polygon;
  for i = 0 to 62 do
    GlDraw.vertex3 (x +. r *.(cos ((float i) /. 10.)) ,
		    y +. r *.(sin ((float i) /. 10.)) , 0.0);
  done;
  GlDraw.ends ()

let draw_circle x y r=
  GlDraw.begins `line_loop;
  for i = 0 to 62 do
    GlDraw.vertex3 (x +. r *.(cos ((float i) /. 10.)) ,
		    y +. r *.(sin ((float i) /. 10.)) , 0.0);
  done;
  GlDraw.ends ()

let draw_trajectory l =
  (*List.iter (fun (x,y,_) -> draw_circle x y 170.) l;*)
  GlDraw.begins `line_strip;
  List.iter (fun (x,y,_) -> GlDraw.vertex3 (x, y, 0.0)) l;
  GlDraw.ends ()


let linescreen (xb,yb) =
  GlDraw.begins `lines;
  GlDraw.vertex3 (!posxscreen, !posyscreen, 0.0);
  GlDraw.vertex3 (xb, yb, 0.0);
  GlDraw.ends ();
  movescreen (xb,yb)

let set_proj x y = 
  GlDraw.viewport ~x:0 ~y:0 ~w:x ~h:y;
  GlMat.mode `projection;
  GlMat.load_identity ();
  (*GluMat.perspective ~fovy:45.0 ~aspect:((float_of_int x) /. (float_of_int y)) ~z:(0.1, 1000.);
  GluMat.look_at ~eye:(10., 0., 0.) ~center:(0., 0., 0.) ~up:(0., 0., 1.);;
  *)
  GluMat.ortho2d ~x:(-10.0,10.0) ~y:(-10.0,10.0)


let draw_string x y s =
  let yr = ref y in 
  GlPix.raster_pos ~x:x ~y:y ~z:(0.0) ();
  for i = 0 to String.length s -1 do
    if s.[i]<>'\n' then 
      Glut.bitmapCharacter ~font:Glut.BITMAP_HELVETICA_18 ~c:(int_of_char s.[i])
    else (
      yr := !yr -. 0.55;
      GlPix.raster_pos ~x:x ~y: !yr ~z:(0.0) ();
    );
  done;;


type color = White | Black | Red
let set_color = function
  | White -> GlDraw.color (1.,1.,1.)
  | Black -> GlDraw.color (0.,0.,0.)
  | Red -> GlDraw.color (1.,0.,0.)

let draw_roomba (x,y,rho) =
  let r = 170. in
  movescreen (x,y);
  set_color White;
  fill_circle x y r;
  set_color Black;
  linescreen (x,y);
  draw_circle x y r;
  set_color Red;
  linescreen (x +. (r *. (cos rho)),y +.(r *. (sin rho)));
  set_color Black;
  movescreen (x,y)

(*
let draw_screen s t r =
  clear_graph ();
  let ad = string_of_available_data s in
  print_text ad;
  draw_roomba r;
  List.iter (fun (x,y,_) -> linescreen (x,y)) t;
  synchronize ()
*)

let render ro () =    
  GlMat.mode `modelview;
  GlMat.load_identity ();
  
  GlMat.push ();

  GlMat.translate ~x: !xtrans ~y: (-. !ytrans) ();
  GlMat.scale ~x: !echelle ~y: !echelle ~z: !echelle  ();
  

  GlClear.clear [`color; `depth ];  

  draw_trajectory !trajectory;
  draw_roomba ( !posx , !posy , !angle );

  GlMat.pop ();

  GlDraw.color (0.,0.,0.);
  draw_string (-10.0) (9.0) (!data_string);

  Gl.flush ();
  Glut.swapBuffers ();;


let xmouse = ref 0
let ymouse = ref 0

let mouseMotion ~x:x ~y:y =
  xtrans := !xtrans +. (float (x- !xmouse))/. 35. ;
  ytrans := !ytrans +. (float (y- !ymouse))/. 35. ;
  xmouse := x;
  ymouse := y


let mouse ~button:button ~state:state ~x:x ~y:y =  
  xmouse := x;
  ymouse := y

	
	  

let keyboard ro kb ~key ~x ~y = 
  match char_of_int key with
    | 'r' ->echelle:= !echelle*.1.1
    | 't' ->echelle:= !echelle/.1.1
    | 'j' -> jostickon := not !jostickon
    | x -> kb ro x;;

let joystick ro jo ~buttonMask:button ~x:x ~y:y ~z:z =
  if !jostickon then
    let zerox = -349
    and zeroy = -239
    and zeroz = -301 in
    let fx = (float (x-zerox)) /. 500.
    and fy = (float (y-zeroy)) /. 500.
    and fz = (float (z-zeroz)) /. 500. in
    jo ro fx fy fz

let init kb jo ms ro =
  let _ = Glut.init Sys.argv in
  Glut.initDisplayMode ~depth:false ~double_buffer:true ~alpha:false ();
  Glut.initWindowSize ~w:700 ~h:700;
  ignore(Glut.createWindow ~title:"Roomba-OCaml-Driver");
  (*Glut.fullScreen ();*)
  Glut.displayFunc ~cb:(render ro);
  Glut.idleFunc ~cb:(Some Glut.postRedisplay);
  set_proj 500 500;
  GlClear.color (0.6, 1., 0.8);
  Glut.keyboardFunc ~cb:(keyboard ro kb);
  Glut.mouseFunc ~cb:(mouse);
  Glut.motionFunc ~cb:(mouseMotion);
(*  Glut.passiveMotionFunc ~cb:mouvmouse;*)
  Glut.joystickFunc ~cb:(joystick ro jo) ~pollInterval:200;
  try ignore (Roomba_ams.start (ms ro)) with
    _ -> print_endline "AMS did not start";
  Glut.mainLoop ()
