
type amsTracker = {
  amsProc: in_channel;
  mutable stop_flag: bool;
  callback: int -> int -> int -> unit;
}

let regexp = Str.regexp "/(-?[0-9]+)[\t]+(-?[0-9]+)[\t]+(-?[0-9]+)/" ;;
let spacesplit = Str.regexp "[ ]";;

let parse_amsline s =
  let l = Str.split spacesplit s in 
  let l2 = List.rev (List.fold_left (fun x y -> if y = "" then x else y::x) [] l) in
  match l2 with
      [x;y;z] -> (int_of_string x,int_of_string y,int_of_string z)
    |_ -> (0,0,0)

let rec ams_loop s =
  let line = input_line s.amsProc in
  if s.stop_flag then 
    Unix.close_process_in s.amsProc
  else 
    let x,y,z = parse_amsline line in
    s.callback x y z;
    ams_loop s

let start f =
  let mouv = Unix.open_process_in "./AMSTracker -u 0.25 -s" in
  let track = {
    amsProc = mouv;
    stop_flag = false;
    callback = f } in
  ignore (Thread.create ams_loop track);
  track

let stop s =
  s.stop_flag <- true

let print_callback x y z =
  Printf.printf "x:%i y:%i z=%i" x y z;
  print_newline ()

(*
  let fz = 2*z in
  roomba_cmd nestor (DriveDirect (2 *(-x-fz)  , 2 *(-fz+x)));
*)
