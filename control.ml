open Type_def
open Interface
open DisplayGLUT
open SmoothMovement

let explore = ref false
let joystick = ref false
let ams = ref false

type innerstate_t = Stop | Avance | Droite | Gauche | AvantDroit1 | AvantDroit2 | Suiveur of int

let string_of_state = function
  | Stop -> "Stop"
  | Avance -> "Avance"
  | Droite -> "Droite"
  | Gauche -> "Gauche"
  | AvantDroit1 -> "AvantDroit1"
  | AvantDroit2 -> "AvantDroit2"
  | Suiveur x -> "Suiveur "^(string_of_int x)

let innerstate = ref Stop;;

let nestor = (
  if Array.length Sys.argv >1 then
    Unix.handle_unix_error  init_roomba Sys.argv.(1)
  else dummy_roomba ());;

let nestor_consigne = start_consigne nestor;;

let changestate ro s =
  if !innerstate = s then () 
  else begin
    innerstate := s;
    match s with
    | Stop ->  change_consigne nestor_consigne 0 0
    | Avance ->  change_consigne nestor_consigne 70 70
    | Droite ->  change_consigne nestor_consigne 50 (-50)
    | Gauche ->  change_consigne_urgent nestor_consigne (-50) (50)
    | AvantDroit1 ->  change_consigne nestor_consigne 70 60
    | AvantDroit2 ->  change_consigne nestor_consigne 70 40
    | Suiveur x->  change_consigne nestor_consigne (70+x) (70-x);
  end


let jo ro fx fy _ = 
  if !joystick then 
  roomba_cmd ro (
    DriveDirect (
      int_of_float (300. *.(fx-.fy))  , 
      int_of_float (300. *.(-.fy-.fx))))
      
let amscall ro fx fy fz =
  if !ams then 
    Roomba_ams.print_callback fx fy fz

let kb ro = function 
      '\027'->begin
	close_roomba ro;
	exit 0
      end
    | 'm' -> roomba_cmd ro (Motors 7)
    | 'l' -> roomba_cmd ro (Motors 0)
    | 's' -> sync_state ro [1;2;3;43;44;45;106];
      print_endline "Sync"
    | 'q' -> stop_sync ro;
    | 'p' -> roomba_cmd ro Clean; 
    | 'c' -> roomba_cmd ro Safe;
      print_endline "Enter Safe Mode";
    | 'd' -> roomba_cmd ro Dock;
(*
    | '5' -> roomba_cmd ro (Drive (0,0))
    | '8' -> roomba_cmd ro (Drive (100,0))
    | '4' -> roomba_cmd ro (Drive (100,1))
    | '6' -> roomba_cmd ro (Drive (100,-1))
    | '2' -> roomba_cmd ro (Drive (-100,0))*)
    | '5' -> change_consigne nestor_consigne 0 0
    | '8' -> change_consigne nestor_consigne 100 100
    | '4' -> change_consigne nestor_consigne 50 (-50)
    | '6' -> change_consigne nestor_consigne (-50) 50
    | '2' -> change_consigne nestor_consigne (-100) (-100)


    | 'e' -> explore := not !explore;
      print_endline "Exploration mode";
      changestate ro Avance; 
      (*changestate ro Stop*)
    | 'j' -> joystick := not !joystick
    | 'k' -> ams := not ! ams
      
    | _->();;

let bound xf y =
  let x = int_of_float xf in
  if x > y then y 
  else if x < -y then -y
  else x

let exp_sign x y =
  if x > 0.0 then x**y
  else -. ((abs_float x)**y)

let explorefun ro st =
  match getDataOrDie st.bumpsWheeldrops with
    | 0 -> begin match getDataOrDie st.lightBumper with
	| lb when lb < 32 && lb > 0-> changestate ro Gauche
	| lb ->
	  let u = float (270 - (getDataOrDie st.lightBumpRight)) in
	  let v = bound ((exp_sign u 1.5)/.100.) 50 in
	  match v with
	      50 -> changestate ro AvantDroit2
	    | -50 -> changestate ro Gauche
	    |  x -> changestate ro (Suiveur x)
    (*begin match getDataOrDie st.lightBumpFrontRight with
	    | i when i > 25 -> changestate ro AvantDroit1
	    | _ -> changestate ro AvantDroit2
	end*)
    end
    | 1 -> changestate ro Gauche 
    | 2 -> changestate ro Gauche
    | 3 -> changestate ro Gauche
    | _ -> changestate ro Stop;;
      
let explorefun2 ro st =
  let v = (270 - (getDataOrDie st.lightBumpRight))/5 in
  change_consigne nestor_consigne (70+v) (70-v)
  (*
  roomba_cmd ro (DriveDirect (70+v,70-v));
  Printf.sprintf "DriveDirect %i,%i\n" (70+v) (70-v)*)

let callback ro st =
  smooth_sensors st;
  let order =
    if !explore then try explorefun ro st; "Explore\n" with 
      | Data_not_available -> 
	print_endline "Some data are not available";
	changestate ro Stop; "\n"
    else "\n"
  in
  callbackfun (order^(string_of_state !innerstate)^"\n") st;;


change_callback nestor (callback nestor);
print_endline "call back";

init kb jo amscall nestor;;
