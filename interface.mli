(** This Module implement the interface with the Roomba robot*)

(** The internal representation a the roomba*)
type roomba 

(** A dummy roomba *)
val dummy_roomba : unit -> roomba

(** Get the sensor state from the Roomba*)
val get_state : roomba -> Type_def.roomba_state

(** Initiate a connection to a roomba robot either locally by
specifying a serial port connected to the roomba or at distance
by connecting to a roomba server*)
val init_roomba : string -> roomba


val roomba_cmd : roomba -> Type_def.cmd -> unit
val close_roomba : roomba -> unit
val clear_input : roomba -> unit
val query_list : roomba -> int list -> unit
  (** 
      @raise Type_def.Upstream_in_use if the roomba state is synchronise
      @raise Type_def.Invalid_Packet if one of the packet require is not valid
  *)

val sync_state : roomba -> int list -> unit
  (**
     @raise Type_def.Invalid_Packet if one of the packet require is not valid
  *)

val stop_sync : roomba -> unit
val wait_end_callback : roomba -> unit
val change_callback : roomba -> ( Type_def.roomba_state -> unit) -> unit
