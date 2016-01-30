open Type_def



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

