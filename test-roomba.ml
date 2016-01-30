#directory "_build"

#load "unix.cma"
#load "graphics.cma"
#load "roomba_lib.cma"


open Interface
open Type_def

let nestor = init_roomba "/dev/rfcomm0";;

print_string (string_of_available_data (get_state nestor));;

Graphics.open_graph "";;
Graphics.read_key ();;

roomba_cmd nestor Safe;;

roomba_cmd nestor (Drive (100,0));;
roomba_cmd nestor (Drive (0,0));;

Display.echelle := 0.2;;
Display.init ();;
List.length !Display.trajectory;;

sync_state nestor [1;2;3;43;44];;

stop_sync nestor;;

close_roomba nestor;;

let s = get_state nestor;;
Distance.movement s;;


roomba_cmd nestor Clean;;
roomba_cmd nestor Power;;


change_callback nestor Display.callbackfun;;

change_callback nestor (fun _ ->());;

clear_input nestor;
get_state nestor;;

while true do try clear_input nestor with _-> () done;;

roomba_cmd nestor Power;;

query_list nestor [43;44];;

let wheeldiffv = wheeldiff (get_state nestor);;

roomba_cmd nestor (Drive (500,1));
change_callback nestor (fun s -> 
  let wdv2 = wheeldiffv - (wheeldiff s) in
  print_int wdv2;
  print_newline ();
  if abs wdv2 > 3270 then (
    roomba_cmd nestor (Drive (0,0));
    change_callback nestor (fun _ -> ())));;

wheeldiffv - (wheeldiff (get_state nestor));;
(* Complet turn over diff = 3350  -> 2*1671 *)


roomba_cmd nestor (Drive (50,0));;

roomba_cmd nestor (Motors 0);;

query_list nestor [43;44];;
get_state nestor;

roomba_cmd nestor (Stream [2]);
let f2 ro =
  roomba_cmd ro (PauseStream false);
  failwith "exit" in
input_sensor nestor f2;;

read_stream nestor;;

close_roomba nestor;;


while true do
roomba_cmd nestor (Leds (31,60,255));
roomba_cmd nestor (Leds (0,128,255));
done;;


roomba_cmd nestor Dock;;

DisplayGLUT.init nestor;;
