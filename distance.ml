open Type_def
  
type trajectory_pt = {
  posx:float;
  posy:float;
  angle:float
}

  
let static_pt = ref { posx=0.0; posy=0.0; angle=0.0}
let static_traj = ref [!static_pt ]
  
     
let rad_of_count = 8.*.atan 1. /. 3350.
let mm_of_count = 0.26819 *. 2.0

let diffEncoder n o = 
  match n-o with
      x when x > 128*256 -> x -256*256
    | x when x < -128*256 -> x +256*256
    | x -> x

let oldLeftEncoder = ref None
let oldRightEncoder = ref None

let diffLeftEncoder r =
  match r.leftEncoder, !oldLeftEncoder with
      None,_ -> raise Data_not_available
    | x, None -> oldLeftEncoder := x; 0
    | Some n , Some o -> let d = diffEncoder n o in
			 oldLeftEncoder := Some n;
			 d

let diffRightEncoder r =
  match r.rightEncoder, !oldRightEncoder with
      None,_ -> raise Data_not_available
    | x, None -> oldRightEncoder := x; 0
    | Some n , Some o -> let d = diffEncoder n o in
			 oldRightEncoder := Some n;
			 d

let movement rs =
  let r = diffRightEncoder rs
  and l = diffLeftEncoder rs in
  (( mm_of_count *. (float (r + l)) /. 2. ) ,
   ( rad_of_count *. (float (r -l))))



let add_trajectory pt =
  match !static_traj with
    [] -> static_traj := [pt]
  | t::_ when pt = t -> ()
  | x -> static_traj := pt::x

let callbackfun ?cb:(cb=fun _ _ _ -> ()) ptref s =
  try
    let pt = !ptref in
    let dist,dangle = movement s in
    let nangle = pt.angle +. dangle in
    let nposx  = pt.posx +. ( dist *. (cos pt.angle )) in
    let nposy  = pt.posy +. ( dist *. (sin pt.angle )) in
    ptref := { posx=nposx; posy=nposy; angle=nangle };
    add_trajectory !ptref;
    cb nposx nposy nangle
  with
    Data_not_available -> ()
  | x -> print_endline "exception in the callback";
    raise x
