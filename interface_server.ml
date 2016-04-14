open Type_def
open Interface_local

type order = 
    Get_state
  | Roomba_cmd of cmd
  | Clear_input
  | Query_list of int list
  | Sync_state of int list
  | Stop_sync
  | Wait_end_callback

let rec loop roomba inchan outchan =
  let ov = output_value outchan in begin
    match input_value inchan with
	Get_state -> ov (get_state roomba)
      | Roomba_cmd cmd -> roomba_cmd roomba cmd
      | Clear_input -> clear_input roomba
      | Query_list l -> 
	query_list roomba l;
	ov (get_state roomba)
      | Sync_state l -> 
	change_callback roomba (fun x ->
	  output_value outchan x;
	  flush outchan
	);
	sync_state roomba l
      | Stop_sync -> stop_sync roomba
      | Wait_end_callback -> wait_end_callback roomba;
  end;
  flush outchan;
  loop roomba inchan outchan

let new_roomba inchan outchan =
  try
    print_endline "New connection";
    let roomba_dev = input_value inchan in
    print_endline ("Connecting to roomba on: "^roomba_dev);
    let roomba = if roomba_dev = "" then init_roomba "/dev/ttyAMA0"
      else init_roomba roomba_dev in
    output_value outchan None;
    flush outchan;
    try
      loop roomba inchan outchan
    with 
      | End_of_file -> 
	close_roomba roomba
      | x -> close_roomba roomba;
	raise x
  with
      x -> 
	output_value outchan (Some x);
	close_out outchan; 
	print_endline "Deconnection";
	raise x
