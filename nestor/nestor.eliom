{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Eliom_parameter
}}

module Nestor_app =
  Eliom_registration.App (
    struct
      let application_name = "nestor"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let actions = [ "safe"; "start"; "power"; "spot"; "clean"; "max" ;
	      "stop"; "avance"; "recule"; "droite"; "gauche"]

let action_services =
  List.map (fun x ->
    (x,Eliom_service.App.service  ~get_params:unit ~path:[x] ())) actions
    
let actions_service_link =
  List.map (fun (x,y) ->
    a y  [ pcdata x; br () ]  ()) action_services


let html_of_data r =
  List.map (fun (n,v) ->
    li [pcdata n ; pcdata ": "; pcdata v]  ) (Type_def.print_list (Interface_local.get_state r))
    
let skeletton bc action =
  let ro = Unix.handle_unix_error Interface_local.init_roomba "/dev/ttyAMA0" in
  Interface_local.roomba_cmd ro WakeUp;
  Interface_local.query_list ro [1;2;3;43;44;45;106];
  
  Lwt.return
        (Eliom_tools.F.html
           ~title:"Nestor"
           ~css:[["css";"nestor.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Nestor !"];
	     div ~a:[a_class ["action"]] actions_service_link ;
	     div ~a:[a_class ["well"]] bc ;
	     div ~a:[a_class ["sensor"]] (ul (html_of_data ro)) ;
           ]))

    
let () =
  Nestor_app.register
    ~service:main_service
    (fun () () -> skeletton [p [pcdata "A, B, C..."]]);
  List.iter (fun (n,s) ->
    Nestor_app.register
      ~service:s
      (fun () () ->
	skeletton [p [pcdata n]] n))
    action_services
