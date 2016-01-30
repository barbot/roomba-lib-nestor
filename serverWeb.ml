open Unix
open Type_def
open Interface


let ro = (
  if Array.length Sys.argv >2 then
    Unix.handle_unix_error  init_roomba Sys.argv.(2)
  else dummy_roomba ())


let print_answer fd = 
  let s = "HTTP/1.1 200 OK\nContent-Type: text/html\nConnnection: close\n\n"
  in let  s2 = s^"<HTML><a href='Avance'>Avance</a><br><a href='Stop'>Stop</a><br><a href=Recule>Recule</a> <br><a href=Safe>Safe</a> 
<br><a href=Droite>Droite</a> </HTML>" in
  ignore (Unix.write fd s2 0 (String.length s2))


let serv fd =
  let str = String.create 1024 in
  ignore (read fd str 0 1024);
  let nslash = String.index str '/' in
  let str2 = String.sub str nslash (String.length str - nslash) in
  let nesp = String.index str2 ' ' in 
  let str3 = String.sub str2 0 nesp in
  print_endline str3;
  
  begin match str3 with
    | "/Safe" -> roomba_cmd ro Safe;
    | "/Avance" -> roomba_cmd ro (Drive (100,0))
    | "/Stop" -> roomba_cmd ro (Drive (0,0))
    | "/Recule" -> roomba_cmd ro (Drive (-100,0))
    | "/Droite" -> roomba_cmd ro (Drive (100,-1))
    | "/Gauche" -> roomba_cmd ro (Drive (100,1))
    | _ -> print_endline "action non reconnu"
  end;

  print_answer fd;
  close fd;;



let init_serv p = 
  let addr = ADDR_INET (inet_addr_any,p) in
  let servsocket = socket PF_INET SOCK_STREAM 0 in
  bind servsocket addr;
  listen servsocket 1;
  servsocket;;


let rec servloop sock =
  let (c,_) = accept sock in
  serv c;
  servloop sock;;

let sock = init_serv (int_of_string Sys.argv.(1)) in
servloop sock;;
