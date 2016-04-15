module type INTERFACE =
sig
  type roomba
  val dummy_roomba : unit -> roomba
  val get_state : roomba -> Type_def.roomba_state

  val init_roomba : string -> roomba
  val roomba_cmd : roomba -> Type_def.cmd -> unit
  val close_roomba : roomba -> unit
  val clear_input : roomba -> unit
  val query_list : roomba -> int list -> unit
  val sync_state : roomba -> int list -> unit
  val stop_sync : roomba -> unit
  val wait_end_callback : roomba -> unit
  val change_callback : roomba -> ( Type_def.roomba_state -> unit) -> unit
end

module MakeUnify ( I1 : INTERFACE ) ( I2 : INTERFACE ) =
struct

  type roomba = Local of I1.roomba | Distant of I2.roomba
  
  let dummy_roomba () = Local (I1.dummy_roomba ())
  
  let get_state = function
    | Local ro1 -> I1.get_state ro1
    | Distant ro2 -> I2.get_state ro2
  
  let init_roomba st =
    if st.[0] = '/' then Local (I1.init_roomba st)
    else Distant (I2.init_roomba st)
      
  let roomba_cmd = function
    | Local ro1 -> I1.roomba_cmd ro1
    | Distant ro2 -> I2.roomba_cmd ro2
      
  let close_roomba = function
    | Local ro1 -> I1.close_roomba ro1
    | Distant ro2 -> I2.close_roomba ro2
      
  let clear_input = function
    | Local ro1 -> I1.clear_input ro1
    | Distant ro2 -> I2.clear_input ro2

  let query_list = function
    | Local ro1 -> I1.query_list ro1
    | Distant ro2 -> I2.query_list ro2

  let sync_state = function
    | Local ro1 -> I1.sync_state ro1
    | Distant ro2 -> I2.sync_state ro2

  let stop_sync = function
    | Local ro1 -> I1.stop_sync ro1
    | Distant ro2 -> I2.stop_sync ro2

  let wait_end_callback = function
    | Local ro1 -> I1.wait_end_callback ro1
    | Distant ro2 -> I2.wait_end_callback ro2

  let change_callback = function
    | Local ro1 -> I1.change_callback ro1
    | Distant ro2 -> I2.change_callback ro2
       
end



module UnifyInterface : INTERFACE = MakeUnify (Interface_local) (Interface_distante)

include UnifyInterface
