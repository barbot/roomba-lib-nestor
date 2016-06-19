
let (|>>) x f = match x with 
    Some y -> f y
  | None -> None

let (|>>>) x f = match x with 
    Some y -> begin try Some (f y) with _ -> None end
  | None -> None

let (|>>|) x v = match x with
    Some y -> y
  | None -> v

let (|<) x f = let () = f x in x
let (|<>|) f (x,y) = f x y

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

let print_charge = function
  | 0 -> "Not charging"
  | 1 -> "Reconditioning Charging"
  | 2 -> "Full Charging"
  | 3 -> "Trickle Charging"
  | 4 -> "Waiting"
  | _ -> "Charging Fault Condition"

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

let print_io = function
  | 0 -> "Off"
  | 1 -> "Passive"
  | 2 -> "Safe"
  | 3 -> "Full"
     
let print_ioption ?unit:(u="") i = (string_of_int i)^u
let print_boption ?unit:(u="") b = (string_of_bool b)^u
let print_soption f x = f x
     
let update_time r =
  r.hidden.time_index <- (r.hidden.time_index +1) mod (Array.length r.hidden.times);
  r.hidden.times.(r.hidden.time_index) <- Unix.gettimeofday ()
  
let freq r =
  let n = Array.length r.hidden.times in
  ((float n) /. (r.hidden.times.(r.hidden.time_index) -. r.hidden.times.((r.hidden.time_index +1) mod n)))

    
let print_list ?prfun:(printfun=fun label x l -> match x with Some y -> (label,y)::l |_ -> l) r =
  let f fp label op l =
    printfun label (op |>>> fp) l in
  List.fold_left (fun x y -> y x) []
  [
  f print_ioption "bumps Wheel drops" r.bumpsWheeldrops;
  f print_boption "wall" r.wall;
  f print_boption "cliff left" r.cliffLeft;
  f print_boption "cliff front left" r.cliffFrontLeft;
  f print_boption "cliff front right" r.cliffFrontRight;
  f print_boption "cliff right" r.cliffRight;
  f print_boption "virtual Wall" r.virtualWall;
  f print_ioption "motor over currents" r.motorOvercurrents;
  f print_ioption "dirt detect" r.dirtDetect;

  f (print_soption print_ir) "ir code" r.irCode;
  f (print_soption print_ir) "ir code left" r.irCodeLeft;
  f (print_soption print_ir) "ir code right" r.irCodeRight;

  f print_ioption "buttons" r.buttons;
  f print_ioption "distance" r.distance;
  f print_ioption "angle" r.angle;

  f (print_soption print_charge) "charging state" r.chargingState ;
  f (print_ioption ~unit:"mV") "voltage" r.voltage;
  f (print_ioption ~unit:"mA") "current" r.current ;
  f (print_ioption ~unit:"°C") "temperature"  r.temperature;
  f (print_ioption ~unit:"mAh") "battery Charge"  r.batteryCharge;
  f (print_ioption ~unit:"mAh") "battery Capacity"  r.batteryCapacity;

  f print_ioption "wall Signal" r.wallSignal;
  f print_ioption "cliff left Signal" r.cliffLeftSignal;
  f print_ioption "cliff front left Signal" r.cliffFrontLeftSignal;
  f print_ioption "cliff front right Signal" r.cliffFrontRightSignal;
  f print_ioption "cliff right Signal" r.cliffRightSignal;
  f print_ioption "charging source" r.chargingSource;

  f (print_soption print_io) "OI Mode" r.oiMode;
  f print_ioption "song Number" r.songNumber;
  f print_boption "song is playing ?" r.songIsPlaying;
  f print_ioption "OI stream Packet Number" r.oiStreamNumPacket;
  f print_ioption "roomba velocity" r.velocity;
  f print_ioption "roomba radius" r.radius;
  f print_ioption "right velocity" r.velocityRight;
  f print_ioption "left velocity" r.velocityLeft;

  f print_ioption "left encoder" r.leftEncoder ;
  f print_ioption "right encoder" r.rightEncoder ;

  
  f print_ioption "bumper" r.lightBumper;
  f print_ioption "bump left Signal" r.lightBumpLeft;
  f print_ioption "bump front left Signal" r.lightBumpFrontLeft;
  f print_ioption "bump center left Signal" r.lightBumpCenterLeft;
  f print_ioption "bump center right Signal" r.lightBumpCenterRight;
  f print_ioption "bump front right Signal" r.lightBumpFrontRight;
  f print_ioption "bump right Signal" r.lightBumpRight;
  
  f (print_ioption ~unit:"mA") "left motor current" r.leftMotorCurrent;
  f (print_ioption ~unit:"mA") "right motor current" r.rightMotorCurrent;
  f (print_ioption ~unit:"mA") "main brush motor current" r.mainBrushMotorCurrent;
  f (print_ioption ~unit:"mA") "side brush motor current" r.sideBrushMotorCurrent;
  f print_boption "stasis" r.stasis;


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
