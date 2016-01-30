
type roomba_mouv

val start_consigne: Interface.roomba -> roomba_mouv
val stop_consigne: roomba_mouv -> unit
val change_consigne: roomba_mouv -> int -> int -> unit
val change_consigne_urgent: roomba_mouv -> int -> int -> unit
