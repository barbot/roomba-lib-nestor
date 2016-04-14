
let max_change = 2.0
let time_change = 0.01

type roomba_mouv = {
  roomba: Interface.roomba;
  mutable consigne_left : int;
  mutable consigne_right : int;
  mutable last_right: int;
  mutable last_left:int;
  mutable last_time: float;
  mutex: Mutex.t;
  mutable thread: Thread.t;
}

let copysignint v s =
  if s>0 then
    abs (int_of_float v)
  else - (abs (int_of_float v))

let send_consigne rm =
  Mutex.lock rm.mutex;
  let t2 = Unix.gettimeofday () in
  let cl = float (rm.consigne_left - rm.last_left) in
  let cr = float (rm.consigne_right - rm.last_right) in
  let maxm = max_change *. (t2 -. rm.last_time) /. time_change in
  let clr = max (abs_float cl) (abs_float cr) in
  if maxm > clr then begin
    rm.last_right <- rm.consigne_right;
    rm.last_left <- rm.consigne_left;
  end else begin
    let p = maxm /. clr in
    rm.last_right <- rm.last_right + int_of_float (cr *. p);
    rm.last_left <- rm.last_left + int_of_float (cl *. p);
  end ;
  rm.last_time <- t2;
  Interface.roomba_cmd rm.roomba (Type_def.DriveDirect (rm.last_left,rm.last_right));
  Mutex.unlock rm.mutex
   
let rec order rm =
  send_consigne rm;
  ignore (Unix.select [] [] [] time_change);
  order rm

let start_consigne ro =
  let rm = {
    roomba = ro;
    consigne_left = 0;
    consigne_right= 0;
    last_right = 0;
    last_left = 0;
    last_time = Unix.gettimeofday ();
    mutex = Mutex.create ();
    thread = Thread.self ()} in
  let t = Thread.create order rm in
  rm.thread <- t;
  rm

let stop_consigne rm = 
  Interface.roomba_cmd rm.roomba (Type_def.Drive (0,0));
  Thread.kill rm.thread

let change_consigne rm x y =
  Mutex.lock rm.mutex;
  rm.consigne_right <- x;
  rm.consigne_left <- y;
  Mutex.unlock rm.mutex

let change_consigne_urgent rm x y =
  Mutex.lock rm.mutex;
  rm.consigne_right <- x;
  rm.consigne_left <- y;
  rm.last_right <- x;
  rm.last_left <- y;
  Mutex.unlock rm.mutex





