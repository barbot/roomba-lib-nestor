open Type_def

let ioc c = int_of_char c
let sioc c =
  Some (int_of_char c)
let sboc c =
  Some ((int_of_char c) =1)
let susoc c1 c2 =
  Some ((ioc c1) * 256 + (ioc c2))
let sdioc c1 c2 =
  let hb = ioc c1 in
  Some ( (hb land 127) * 256 + (ioc c2) - (hb land 128)*256 )

let packet_length = function
  | 0 -> 10+6+10
  | 1 -> 10
  | 2 -> 6
  | 3 -> 10
  | 4 -> 14
  | 5 -> 12
  | 6 -> 26+14+12
  | x when x>=7 && x<=18 -> 1
  | 19 -> 2
  | 20 -> 2
  | 21 -> 1
  | 22 -> 2
  | 23 -> 2
  | 24 -> 1
  | x when x>=25 && x <= 31 -> 2
  | 32 -> 1
  | 33 -> 2
  | x when x>=34 && x <= 38 -> 1
  | x when x>=39 && x <= 44 -> 2
  | 45 -> 1
  | x when x>=46 && x <= 51 -> 2
  | 52 -> 1
  | 53 -> 1
  | x when x>=54 && x <= 57 -> 2
  | 58 -> 1

  | 100 -> 26+14+12 + 5+12+2+8
  | 101 -> 5+12+2+8
  | 106 -> 12
  | 107 -> 8

  |x-> raise (Invalid_Packet x)

  
let rec import_group rs s i l =
  let _ = 
    List.fold_left (fun x y -> 
      import_packet rs s x y;
      x + packet_length y) i l in ()
and import_packet rs s i = function
  | 0 -> import_group rs s i [1;2;3]
  | 1 -> import_group rs s i [7;8;9;10;11;12;13;14;15]
  | 2 -> import_group rs s i [17;18;19;20]
  | 3 -> import_group rs s i [21;22;23;24;25;26]
  | 4 -> import_group rs s i [27;28;29;30;31;34]
  | 5 -> import_group rs s i [35;36;37;38;39;40;41;42]
  | 6 -> import_group rs s i [0;4;5]

  | 7 -> rs.bumpsWheeldrops <- sioc s.[i]
  | 8 -> rs.wall <- sboc s.[i]
  | 9 -> rs.cliffLeft <- sboc s.[i]
  | 10-> rs.cliffFrontLeft <- sboc s.[i]
  | 11-> rs.cliffFrontRight <- sboc s.[i]
  | 12-> rs.cliffRight <- sboc s.[i]
  | 13-> rs.virtualWall <- sboc s.[i]
  | 14-> rs.motorOvercurrents <- sioc s.[i]
  | 15-> rs.dirtDetect <- sioc s.[i]
  | 16-> ()
  | 17-> rs.irCode <- sioc s.[i]
  | 18-> rs.buttons <- sioc s.[i]
  | 19-> rs.distance <- sdioc s.[i] s.[i+1]
  | 20-> rs.angle <- sdioc s.[i] s.[i+1]
  | 21-> rs.chargingState <- sioc s.[i]
  | 22-> rs.voltage <- susoc s.[i] s.[i+1]
  | 23-> rs.current <- sdioc s.[i] s.[i+1]
  | 24-> rs.temperature <- sioc s.[i]
  | 25-> rs.batteryCharge <- susoc s.[i] s.[i+1]
  | 26-> rs.batteryCapacity <- susoc s.[i] s.[i+1]
  | 27-> rs.wallSignal <- susoc s.[i] s.[i+1]
  | 28-> rs.cliffLeftSignal <- susoc s.[i] s.[i+1]
  | 29-> rs.cliffFrontLeftSignal <- susoc s.[i] s.[i+1]
  | 30-> rs.cliffFrontRightSignal <- susoc s.[i] s.[i+1]
  | 31-> rs.cliffRightSignal <- susoc s.[i] s.[i+1]
  | 32-> ()
  | 33-> ()
  | 34-> rs.chargingSource <- sioc s.[i]
  | 35-> rs.oiMode <- sioc s.[i]
  | 36-> rs.songNumber <- sioc s.[i]
  | 37-> rs.songIsPlaying <- sboc s.[i]
  | 38-> rs.oiStreamNumPacket <- sioc s.[i]
  | 39-> rs.velocity<- sdioc s.[i] s.[i+1]
  | 40-> rs.radius<- sdioc s.[i] s.[i+1]
  | 41-> rs.velocityRight<- sdioc s.[i] s.[i+1]
  | 42-> rs.velocityLeft<- sdioc s.[i] s.[i+1]
  | 43-> rs.leftEncoder <- susoc s.[i] s.[i+1]
  | 44-> rs.rightEncoder <- susoc s.[i] s.[i+1]
  | 45-> rs.lightBumper <- sioc s.[i]
  | 46-> rs.lightBumpLeft <- susoc s.[i] s.[i+1]
  | 47-> rs.lightBumpFrontLeft <- susoc s.[i] s.[i+1]
  | 48-> rs.lightBumpCenterLeft <- susoc s.[i] s.[i+1]
  | 49-> rs.lightBumpCenterRight <- susoc s.[i] s.[i+1]
  | 50-> rs.lightBumpFrontRight <- susoc s.[i] s.[i+1]
  | 51-> rs.lightBumpRight <- susoc s.[i] s.[i+1]
  | 52-> rs.irCodeLeft <- sioc s.[i]
  | 53-> rs.irCodeRight <- sioc s.[i]
  | 54-> rs.leftMotorCurrent <- sdioc s.[i] s.[i+1]
  | 55-> rs.rightMotorCurrent <- sdioc s.[i] s.[i+1]
  | 56-> rs.mainBrushMotorCurrent <- sdioc s.[i] s.[i+1]
  | 57-> rs.sideBrushMotorCurrent <- sdioc s.[i] s.[i+1]
  | 58-> rs.stasis <- sboc s.[i]
 
  | 100-> import_group rs s i [6;101]
  | 101-> import_group rs s i [43;45;106;52;54;107]
  | 106-> import_group rs s i [46;47;48;49;50;51]
  | 107-> import_group rs s i [54;55;56;57;58]

  |x -> raise (Invalid_Packet x)
