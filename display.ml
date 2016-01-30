open Graphics
open Type_def

let echelle = ref 0.02
let origine = ref (1400.0,600.0) 

let time_draw = ref (Unix.gettimeofday ())

let convert (x,y) =
  let xo,yo = !origine in
  (int_of_float ((x+.xo) *. !echelle) , 
   int_of_float ((y+.yo) *. !echelle ))

let movescreen a =
  let x,y = convert a in
  moveto x y

let linescreen a =
  let x,y = convert a in
  lineto x y

let rec sleepl i =
  try ignore (Unix.select [] [] [] (float (i*1000))) with
      Unix.Unix_error(Unix.EAGAIN,_,_) -> 
	sleepl i
    | Unix.Unix_error(Unix.EINTR,_,_) -> 
      sleepl i
    | Unix.Unix_error(Unix.EUNKNOWNERR(260),_,_) -> 
      sleepl i; 
    | Unix.Unix_error(j,_,_) -> 
      print_endline "uncaught sleep";
      print_int (Obj.magic j);
      sleepl i; 
    | x -> print_endline "uncaught sleep";
      raise x

let draw_roomba (x,y,rho) =
  let xi,yi = convert (x,y) in
  let r = int_of_float (170. *. !echelle) in
  movescreen (x,y);
  set_color white;
  fill_circle xi yi r;
  set_color black;
  lineto xi yi;
  draw_circle xi yi r;
  set_color red;
  linescreen (x +. (170. *. (cos rho)),y +.(170. *. (sin rho)));
  set_color black;
  movescreen (x,y)

let print_text s=
  moveto 10 (size_y () -20);
  let endsplit = Str.regexp "\n" in
  List.iter (fun x -> 
    draw_string x;
    let dx,dy = text_size x in
    rmoveto (-dx) (-dy)) (Str.split endsplit s)


let draw_screen s t r =
  clear_graph ();
  let ad = string_of_available_data s in
  print_text ad;
  draw_roomba r;
  List.iter (fun (x,y,_) -> linescreen (x,y)) t;
  synchronize ()


let angle = ref 0.0
let posx = ref 700.0 
let posy = ref 300.0
let trajectory = ref []

let add_trajectory pt =
  match !trajectory with
      [] -> trajectory := [pt]
    | t::_ when pt = t -> ()
    | x -> trajectory := pt::x

let kbfun = ref (fun c -> ())

let callbackfun str s =
  if Graphics.key_pressed ()
  then 
    let v = Graphics.read_key () in
    !kbfun v;
  try
    let dist,dangle = Distance.movement s in
    angle := !angle +. dangle;
    posx := !posx +. ( dist *. (cos !angle ));
    posy := !posy +. ( dist *. (sin !angle ));
    add_trajectory (!posx, !posy, !angle);
    let t = Unix.gettimeofday () in
    if t-. !time_draw > 0.1 then (
      time_draw :=t;
      draw_screen s !trajectory (!posx, !posy, !angle)
    )
  with
      Data_not_available -> ()
    | x -> print_endline "exception in the callback";
	raise x

(*let callbackcmd ro kb s =
  callbackfun s;
  if Graphics.key_pressed ()
  then let v = Graphics.read_key () in
       Printf.printf "Key %c pressed" v;
       print_newline ();
       kb ro v
*)

let simplecallback _ _ s =
  print_endline (Type_def.string_of_available_data s)


let init kb _ ro = 
  open_graph " 1024x768";
  auto_synchronize false;
  clear_graph ();
  echelle := 0.1;
  origine := (5000. , 3000.);
  kbfun := kb ro

(*
  try
    sync_state ro [1;2;3;43;44];
    print_endline "Sync";
    
    (*change_callback nestor (simplecallback nestor);*)
    change_callback ro (callbackcmd ro kb);
    print_endline "call back";
    
    (*roomba_cmd nestor Clean;
    print_endline "clean";*)

    wait_end_callback ro;

    close_roomba ro;
  with 
      x -> close_roomba ro; raise x;;
*)
