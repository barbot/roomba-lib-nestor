(** This Module describe the state of the sensor of the Roomba*)
type smooth_value

type roomba_state = {
  mutable bumpsWheeldrops : int option;
  mutable wall : bool option;
  mutable cliffLeft : bool option;
  mutable cliffFrontLeft : bool option;
  mutable cliffFrontRight : bool option;
  mutable cliffRight : bool option;
  mutable virtualWall : bool option;
  mutable motorOvercurrents : int option;
  mutable dirtDetect : int option;
  mutable irCode : int option;
  mutable buttons : int option;
  mutable distance : int option;
  mutable angle : int option;
  mutable chargingState : int option;
  mutable voltage : int option;
  mutable current : int option;
  mutable temperature : int option;
  mutable batteryCharge : int option;
  mutable batteryCapacity : int option;
  mutable wallSignal: int option;
  mutable cliffLeftSignal: int option;
  mutable cliffFrontLeftSignal : int option;
  mutable cliffFrontRightSignal : int option;
  mutable cliffRightSignal : int option;
  mutable chargingSource : int option;
  mutable oiMode : int option;
  mutable songNumber : int option;
  mutable songIsPlaying : bool option;
  mutable oiStreamNumPacket : int option;
  mutable velocity : int option;
  mutable radius : int option;
  mutable velocityRight : int option;
  mutable velocityLeft : int option;
  mutable leftEncoder : int option;
  mutable rightEncoder : int option;
  mutable lightBumper : int option;
  mutable lightBumpLeft : int option;
  mutable lightBumpFrontLeft : int option;
  mutable lightBumpCenterLeft : int option;
  mutable lightBumpCenterRight : int option;
  mutable lightBumpFrontRight : int option;
  mutable lightBumpRight : int option;
  mutable irCodeLeft:int option;
  mutable irCodeRight:int option;
  mutable leftMotorCurrent: int option;
  mutable rightMotorCurrent: int option;
  mutable mainBrushMotorCurrent: int option;
  mutable sideBrushMotorCurrent: int option;
  mutable stasis: bool option;
  mutable hidden : smooth_value;
}


(** The Command which can be used with the Roomba*)
type cmd = 
    Start
  | Control
  | Safe
  | Full
  | Power
  | Spot
  | Clean
  | Max
  | Drive of int * int
  | Motors of int
  | Leds of int*int*int
  | Dock
  | DriveDirect of int*int
  | Stream of int list
  (** This command should not be used directly use [sync_state] instead.*)
  | PauseStream of bool
  (** This command should not be used directly use [sync_state] and [stop_sync] instead.*)   
  | QueryList of int list
(** This command should not be used directly use [query_list] instead.*)
  | WakeUp


exception Invalid_Packet of int
(** This exception is raised when a required packet is not a valid one*)
    
exception Data_not_available
(** This exception is raised when the value of a sensor is needed but not available*)

exception Upstream_in_use
(** This exception is raised when concurent use of [Roomba_lib.sync_state] and [Roomba_lib.query_list] *)

val emptystate : roomba_state
val clear_state : roomba_state -> unit

val getDataOrDie : 'a option -> 'a
val print_list : roomba_state -> (string * string) list
val string_of_available_data : roomba_state -> string
val update_time : roomba_state -> unit
val freq : roomba_state -> float

val smooth_sensors: roomba_state -> unit
