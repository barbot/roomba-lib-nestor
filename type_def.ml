type smooth_value = {
  mutable lightBumpLeftSm : float;
  mutable lightBumpFrontLeftSm : float;
  mutable lightBumpCenterLeftSm : float;
  mutable lightBumpCenterRightSm : float;
  mutable lightBumpFrontRightSm : float;
  mutable lightBumpRightSm : float;

  (* Frequence of sync *)
  times: float array;
  mutable time_index: int;
}

type roomba_state = {

  (* Packet 1 *)
  mutable bumpsWheeldrops:int option ;
  mutable wall: bool option;
  mutable cliffLeft: bool option;
  mutable cliffFrontLeft: bool option;
  mutable cliffFrontRight: bool option;
  mutable cliffRight: bool option;
  mutable virtualWall: bool option;
  mutable motorOvercurrents: int option;
  mutable dirtDetect: int option; 

  (* Packet 2*)
  mutable irCode: int option;
  mutable buttons: int option;
  mutable distance: int option;
  mutable angle: int option;

  (*packet 3*)
  mutable chargingState:int option;
  mutable voltage:int option;
  mutable current:int option;
  mutable temperature: int option;
  mutable batteryCharge: int option;
  mutable batteryCapacity: int option;

  (*packet 4*)
  mutable wallSignal: int option;
  mutable cliffLeftSignal: int option;
  mutable cliffFrontLeftSignal : int option;
  mutable cliffFrontRightSignal : int option;
  mutable cliffRightSignal : int option;
  mutable chargingSource : int option;

  (*packet 5*)
  mutable oiMode : int option;
  mutable songNumber : int option;
  mutable songIsPlaying : bool option;
  mutable oiStreamNumPacket : int option;
  mutable velocity : int option;
  mutable radius : int option;
  mutable velocityRight : int option;
  mutable velocityLeft : int option;

  (* Encoder *)
  mutable leftEncoder: int option;
  mutable rightEncoder: int option;


  mutable lightBumper: int option;

  (*packet 106*)
  mutable lightBumpLeft : int option;
  mutable lightBumpFrontLeft : int option;
  mutable lightBumpCenterLeft : int option;
  mutable lightBumpCenterRight : int option;
  mutable lightBumpFrontRight : int option;
  mutable lightBumpRight : int option;

  (*no specific packet*)
  mutable irCodeLeft:int option;
  mutable irCodeRight:int option;

  (*packet 107*)
  mutable leftMotorCurrent: int option;
  mutable rightMotorCurrent: int option;
  mutable mainBrushMotorCurrent: int option;
  mutable sideBrushMotorCurrent: int option;
  mutable stasis: bool option;

  mutable hidden: smooth_value;
}


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
  | PauseStream of bool
  | QueryList of int list
  | WakeUp

let print_ioption ?unit:(u="") s2 op l = match op with
  | None -> l
  | Some i -> (s2,(string_of_int i)^u)::l
let print_boption ?unit:(u="") s2 op l = match op with
  | None -> l
  | Some i -> (s2,(string_of_bool i)^u)::l
let print_charge_state s2 op l = match op with
  | None -> l
  | Some 0 -> (s2,"Not charging")::l
  | Some 1 -> (s2,"Reconditioning Charging")::l
  | Some 2 -> (s2,"Full Charging")::l
  | Some 3 -> (s2,"Trickle Charging")::l
  | Some 4 -> (s2,"Waiting")::l
  | Some _ -> (s2,"Charging Fault Condition")::l

let print_ir = function
  | 0 -> ""

  | 162 -> "Virtual Wall"
     
  | 160 -> "Reserved"
  | 161 -> "Force Field"
  | 164 -> "Green Buoy"
  | 165 -> "Green Buoy and Force Field"
  | 168 -> "Red Buoy"
  | 169 -> "Red Buoy and Force Field"
  | 172 -> "Red Buoy and Green Buoy"
  | 173 -> "Red Buoy, Green Buoy and Force Field"
     
  | 240 -> "Reserved"
  | 248 -> "Red Buoy"
  | 244 -> "Green Buoy"
  | 242 -> "Force Field"
  | 252 -> "Red Buoy and Green Buoy"
  | 250 -> "Red Buoy and Force Field"
  | 246 -> "Green Buoy and Force Field"
  | 254 -> "Red Buoy, Green Buoy and Force Field"

  | x -> "Unkown "^(string_of_int x)

let print_ir_code s2 op l = match op with
  | None -> l
  | Some x -> (s2, print_ir x)::l
     
let update_time r =
  r.hidden.time_index <- (r.hidden.time_index +1) mod (Array.length r.hidden.times);
  r.hidden.times.(r.hidden.time_index) <- Unix.gettimeofday ()
  
let freq r =
  let n = Array.length r.hidden.times in
  ((float n) /. (r.hidden.times.(r.hidden.time_index) -. r.hidden.times.((r.hidden.time_index +1) mod n)))

let print_list r =
  List.fold_left (fun x y -> y x) []
  [
  print_ioption "bumps Wheel drops" r.bumpsWheeldrops;
  print_boption "wall" r.wall;
  print_boption "cliff left" r.cliffLeft;
  print_boption "cliff front left" r.cliffFrontLeft;
  print_boption "cliff front right" r.cliffFrontRight;
  print_boption "cliff right" r.cliffRight;
  print_boption "virtual Wall" r.virtualWall;
  print_ioption "motor over currents" r.motorOvercurrents;
  print_ioption "dirt detect" r.dirtDetect;

  print_ir_code "ir code" r.irCode;
  print_ir_code "ir code left" r.irCodeLeft;
  print_ir_code "ir code right" r.irCodeRight;

  print_ioption "buttons" r.buttons;
  print_ioption "distance" r.distance;
  print_ioption "angle" r.angle;

  print_charge_state "charging state" r.chargingState ;
  print_ioption "voltage" ~unit:"mV" r.voltage;
  print_ioption "current" ~unit:"mA" r.current ;
  print_ioption "temperature" ~unit:"°C" r.temperature;
  print_ioption "battery Charge" ~unit:"mAh" r.batteryCharge;
  print_ioption "battery Capacity" ~unit:"mAh" r.batteryCapacity;

  print_ioption "wall Signal" r.wallSignal;
  print_ioption "cliff left Signal" r.cliffLeftSignal;
  print_ioption "cliff front left Signal" r.cliffFrontLeftSignal;
  print_ioption "cliff front right Signal" r.cliffFrontRightSignal;
  print_ioption "cliff right Signal" r.cliffRightSignal;
  print_ioption "charging source" r.chargingSource;

  print_ioption "OI Mode" r.oiMode;
  print_ioption "song Number" r.songNumber;
  print_boption "song is playing ?" r.songIsPlaying;
  print_ioption "OI stream Packet Number" r.oiStreamNumPacket;
  print_ioption "roomba velocity" r.velocity;
  print_ioption "roomba radius" r.radius;
  print_ioption "right velocity" r.velocityRight;
  print_ioption "left velocity" r.velocityLeft;

  print_ioption "left encoder" r.leftEncoder ;
  print_ioption "right encoder" r.rightEncoder ;

  
  print_ioption "bumper" r.lightBumper;
  print_ioption "bump left Signal" r.lightBumpLeft;
  print_ioption "bump front left Signal" r.lightBumpFrontLeft;
  print_ioption "bump center left Signal" r.lightBumpCenterLeft;
  print_ioption "bump center right Signal" r.lightBumpCenterRight;
  print_ioption "bump front right Signal" r.lightBumpFrontRight;
  print_ioption "bump right Signal" r.lightBumpRight;
  
  print_ioption "left motor current" ~unit:"mA" r.leftMotorCurrent;
  print_ioption "right motor current" ~unit:"mA" r.rightMotorCurrent;
  print_ioption "main brush motor current" ~unit:"mA" r.mainBrushMotorCurrent;
  print_ioption "side brush motor current" ~unit:"mA" r.sideBrushMotorCurrent;
  print_boption "stasis" r.stasis;


  (fun l -> ("Periode",Printf.sprintf "%f" (freq r)) :: l);
]


let string_of_available_data r =
  List.fold_left (fun x (n,v) -> Printf.sprintf "%s:%s\n%s" n v x  ) "" (print_list r)

exception Invalid_Packet of int
exception Data_not_available
exception Upstream_in_use

let getDataOrDie = function
  | None -> raise Data_not_available
  | Some x -> x

let emptysmooth = {
  lightBumpLeftSm = 0.0;
  lightBumpFrontLeftSm = 0.0;
  lightBumpCenterLeftSm = 0.0;
  lightBumpCenterRightSm = 0.0;
  lightBumpFrontRightSm = 0.0;
  lightBumpRightSm = 0.0;

  times = Array.make 20 0.;
  time_index = 0;
}

let emptystate={
  bumpsWheeldrops = None;
  wall = None;
  cliffLeft = None;
  cliffFrontLeft = None;
  cliffFrontRight = None;
  cliffRight = None;
  virtualWall = None;
  motorOvercurrents = None;
  dirtDetect = None;

  irCode = None;
  buttons = None;
  distance = None;
  angle = None;

  chargingState = None;
  voltage = None;
  current = None;
  temperature = None;
  batteryCharge = None;
  batteryCapacity = None;

  wallSignal = None;
  cliffLeftSignal = None;
  cliffFrontLeftSignal = None;
  cliffFrontRightSignal = None;
  cliffRightSignal = None;
  chargingSource = None;

  irCodeLeft = None;
  irCodeRight = None;


  oiMode =None;
  songNumber =None;
  songIsPlaying =None;
  oiStreamNumPacket =None;
  velocity =None;
  radius =None;
  velocityRight =None;
  velocityLeft =None;

  lightBumper=None;
  lightBumpLeft = None;
  lightBumpFrontLeft  = None;
  lightBumpCenterLeft = None;
  lightBumpCenterRight  = None;
  lightBumpFrontRight  = None;
  lightBumpRight  = None;

  leftEncoder = None;
  rightEncoder = None;

  leftMotorCurrent = None;
  rightMotorCurrent = None;
  mainBrushMotorCurrent = None;
  sideBrushMotorCurrent = None;
  stasis = None;

  hidden = emptysmooth
}

let clear_state s =
  s.bumpsWheeldrops <- None;
  s.wall  <- None;
  s.cliffLeft  <-None;
  s.cliffFrontLeft  <- None;
  s.cliffFrontRight  <- None;
  s.cliffRight  <- None;
  s.virtualWall  <- None;
  s.motorOvercurrents  <- None;
  s.dirtDetect  <- None;

  s.irCode  <- None;
  s.buttons <- None;
  s.distance <- None;
  s.angle <- None;

  s.chargingState <- None;
  s.voltage <- None;
  s.current <- None;
  s.temperature <- None;
  s.batteryCharge <- None;
  s.batteryCapacity <- None;

  s.wallSignal  <- None;
  s.cliffLeftSignal  <- None;
  s.cliffFrontLeftSignal  <- None;
  s.cliffFrontRightSignal  <- None;
  s.cliffRightSignal  <- None;
  s.chargingSource  <- None;

  s.oiMode <- None;
  s.songNumber <- None;
  s.songIsPlaying <- None;
  s.oiStreamNumPacket <- None;
  s.velocity <- None;
  s.radius <- None;
  s.velocityRight <- None;
  s.velocityLeft <- None;

  s.lightBumper <- None;
  s.lightBumpLeft <- None;
  s.lightBumpFrontLeft <- None;
  s.lightBumpCenterLeft <- None;
  s.lightBumpCenterRight <- None;
  s.lightBumpFrontRight <- None;
  s.lightBumpRight <- None;

  s.leftEncoder <- None;
  s.rightEncoder <- None;

  s.leftMotorCurrent <- None;
  s.rightMotorCurrent <- None;
  s.mainBrushMotorCurrent <- None;
  s.sideBrushMotorCurrent <- None;
  s.stasis <- None


let integre sx y = match sx with
  | None -> None,y
  | Some x ->
    let a = 0.8 in
    (Some (int_of_float (y *. (1.-. a)))
       , (float x) +. a*. y) 
    

let smooth_sensors st =
  let x,y = integre st.lightBumpLeft st.hidden.lightBumpLeftSm in
  st.lightBumpLeft <- x; 
  st.hidden.lightBumpLeftSm <- y;
  let x,y = integre st.lightBumpFrontLeft st.hidden.lightBumpFrontLeftSm in
  st.lightBumpFrontLeft <- x;
  st.hidden.lightBumpFrontLeftSm <- y;
  let x,y = integre st.lightBumpCenterLeft st.hidden.lightBumpCenterLeftSm in
  st.lightBumpCenterLeft <- x;
  st.hidden.lightBumpCenterLeftSm <- y;
  let x,y = integre st.lightBumpRight st.hidden.lightBumpRightSm in
  st.lightBumpRight <- x; 
  st.hidden.lightBumpRightSm <- y;
  let x,y = integre st.lightBumpFrontRight st.hidden.lightBumpFrontRightSm in
  st.lightBumpFrontRight <- x;
  st.hidden.lightBumpFrontRightSm <- y;
  let x,y = integre st.lightBumpCenterRight st.hidden.lightBumpCenterRightSm in
  st.lightBumpCenterRight <- x;
  st.hidden.lightBumpCenterRightSm <- y
