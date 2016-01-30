type order =
     Get_state
  | Roomba_cmd of Type_def.cmd
  | Clear_input
  | Query_list of int list
  | Sync_state of int list
  | Stop_sync
  | Wait_end_callback

val new_roomba: in_channel -> out_channel -> unit
