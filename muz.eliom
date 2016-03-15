{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}

type user = {
  username : string option;
  email: string option;
}

module Config =
  struct
    (* port access to the website *)
    let port = 8081
  end

module Muz_app =
  Eliom_registration.App (
    struct
      let application_name = "muz"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

(* New Account Service *)
let new_account_service =
  Eliom_service.Http.service ~path:["new_account"] ~get_params:Eliom_parameter.unit ()

(* Write the New account info to the database *)
let new_acct_db_service =
  Eliom_service.Http.post_service ~fallback:new_account_service
                                  ~post_params:(string "new_username" **
                                                string "new_email" **
                                                string "new_password" **
                                                string "verify_new_password") ()

(*** Page Elements ***)

(* Bootstrap CDN link *)
let bootstrap_cdn_link =
    let cdn_link = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" in
      link ~rel:[`Stylesheet] ~href:(Xml.uri_of_string cdn_link)
        ()

(* FontAwesome CDN link *)
let font_awesome_cdn_link =
    let cdn_link = "//netdna.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css" in
      link ~rel:[`Stylesheet] ~href:(Xml.uri_of_string cdn_link)
        ()

let new_account_form =
  Eliom_content.Html5.F.post_form ~service:new_acct_db_service ~port:Config.
  (
    fun (new_username, (new_email, (new_password, verify_new_password))) ->
      [div ~a:[a_style "width: 600px; margin: auto"]
       [div ~a:[a_class ["panel panel-primary"];
                a_style "border: 1px solid #634271; width: 400px; margin: auto; border-radius: 4px"]
        [div ~a:[a_class ["panel-heading"];
                 a_style "background-color: #634271; border: 1px solid #634271; border-radius: 0px"]
         [h3 ~a:[a_class ["panel-title"; "text-center"]] [pcdata "Register New Account"]
         ];

         div ~a:[a_class ["panel-body"]; a_style "border-radius: 4px; background: whitesmoke"]
         [
          div ~a:[a_class ["form-group"]]
          [div ~a:[a_class ["input-group"]]
           [Raw.span ~a:[a_class ["input-group-addon"]]
            [Raw.span ~a:[a_class ["glyphicon glyphicon-user"]] []
            ];
            string_input ~a:[a_class ["form-control"]; a_placeholder "Username"]
                         ~input_type:`Text ~name:new_username ()
           ]
          ];

          div ~a:[a_class ["form-group"]]
          [div ~a:[a_class ["input-group"]]
           [Raw.span ~a:[a_class ["input-group-addon"]]
            [Raw.span ~a:[a_class ["glyphicon glyphicon-envelope"]] []
            ];
            string_input ~a:[a_class ["form-control"]; a_placeholder "Email Address (optional)"]
                         ~input_type:`Text ~name:new_email ()
           ]
          ];

           div ~a:[a_class ["form-group"]]
           [div ~a:[a_class ["input-group"]]
            [Raw.span ~a:[a_class ["input-group-addon"]]
             [Raw.span ~a:[a_class ["glyphicon glyphicon-flag"]] []
             ];
             string_input ~a:[a_class ["form-control"]; a_placeholder "Password"]
                          ~input_type:`Password ~name:new_password ()
            ]
           ];

           div ~a:[a_class ["form-group"]]
           [div ~a:[a_class ["input-group"]]
            [Raw.span ~a:[a_class ["input-group-addon"]]
             [Raw.span ~a:[a_class ["glyphicon glyphicon-flag"]] []
             ];
             string_input ~a:[a_class ["form-control"]; a_placeholder "Verify Password"]
                          ~input_type:`Password ~name:verify_new_password ()
            ]
           ];

          button ~a:[a_class ["btn btn-lg btn-success btn-block"];
                     a_style "width: 150px; margin: auto; background-color: #634271;
                              border-color: #634271; font-size: 16px"]
                 ~button_type:`Submit [pcdata "Submit"]
      ]]
      ]]
  )

(* Header Navbar Skeleton *)
let header_navbar_skeleton ?(on_page = `Null) (u : user) =
  let b1 = if on_page = `NewAccount then [] else [new_account_button u] in
  let b2 = if on_page = `Login then [] else [login_logout_button u] in
  let b3 = if on_page = `OrderHistory then [] else [order_history_button u] in
  (*let b4 = if on_page = `Trade then [] else [trade_button u] in*)
  let b5 = if on_page = `FAQ then [] else [faq_button] in
  let btns =
    match on_page with
    | `NewAccount -> b2 @ b5
    | `Login -> b1 @ b5
    | `OrderHistory -> b2 @ b5
    | `Trade -> b2 @ b3 @ b5
    | `FAQ -> b1 @ b2
    | `Logout -> b1 @ b2 @ b5
    | `Null -> b1 @ b2 @ b5 @ [anon_contact_button]
  in
  nav ~a:[a_class ["navbar navbar-fixed-top"]; a_style "background-color: #333;"]
    [div ~a:[a_class ["container-fluid"]] [div ~a:[a_class ["navbar-header"]] btns]]

(*** Register Services ***)

(* Main Page Service *)
let () =
  Muz_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"muz"
           ~css:[["css";"muz.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's distillery!"];
             ])))

(* New Account Service *)
let () =
  Eliom_registration.Html5.register
    ~service:new_account_service
    (fun () () ->
      (* Kick off the thread *)
      Lwt.return @@ Eliom_reference.Volatile.get user_info
      >>= fun user ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"New Account"
           ~css:[["css";"muz.css"]]
           ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
           (body ~a:[a_class ["transparent"]]
           [header_navbar_skeleton ~on_page:`NewAccount user;
            div ~a:[a_style "margin-top: 80px"]
            [h5 ~a:[a_style "text-align: center; margin-bottom: 30px"]
             [pcdata ("Your password must have 8 to 100 characters, " ^
                      "1 uppercase character, 3 numbers and no spaces.")];
             new_account_form ()
            ]
           ])))

(* New Account Database Service - write the new user to the database *)
let () =
  Eliom_registration.Html5.register
    ~service:new_acct_db_service
    (fun () (new_username, (new_email, (new_password, verify_new_password))) ->
      (* Kick off the thread *)
      lwt username_taken = Db_funs.username_exists new_username in
      lwt email_taken =
        if new_email = "" then (Lwt.return false) else Db_funs.email_exists new_email
      in
      let password_verified = (new_password = verify_new_password) in
      let pwd_complexity, pwd_complexity_msg = Db_funs.pwd_req_check new_password in
      lwt user_registration_msg =
        if (not username_taken) && (not email_taken) && password_verified && pwd_complexity
        then
          (
            let new_user = {username = Some new_username; email = Some new_email} in
            let msg = Db_funs.write_new_user new_user new_password in
            (* It is ok to force verified=true since it is only for pub addr creation *)
            let t_id = Db_funs.get_trader_id ~verified:true new_username in
            let new_user' = {username = new_user.username; email = new_user.email} in
            msg
          )
        else
          Lwt.return
          (
            match username_taken, email_taken, password_verified, pwd_complexity with
            | true, _, _, _ -> "The username is not available."
            | false, true, _, _ -> "The email address is already registered."
            | false, false, false, _ -> "The password fields do not match"
            | false, false, true, false -> pwd_complexity_msg
            | false, false, true, true -> "Error: Please contact exchange support."
          )
      in
      let user = Eliom_reference.Volatile.get user_info in
      if (not username_taken) && (not email_taken) && password_verified && pwd_complexity
      then begin
        Lwt.return
          (Eliom_tools.F.html
             ~title:"Muz"
             ~css:[["css"; "muz.css"]]
             ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
             (body ~a:[a_class ["transparent"]]
             [header_navbar_skeleton ~on_page:`NewAccount user;
              div ~a:[a_class ["margin_top_50px"; "padding_top_50px"]]
              [h3 ~a:[a_style "margin: auto auto 20px; text-align: center"]
               [pcdata ("Thanks for registering! Click the button below to begin doing stuff.")];
               div ~a:[a_id "trade_btn_1_div"]
               [div ~a:[a_class ["btn btn-lg btn-success"]; a_id "trade_btn_1";
                        a_style "background-color: #634271; border-color: #634271"]
                [h1 [pcdata "A Button should go here..."] ()]
               ];
              ]
             ]))
      end
      else
        Lwt.return
          (Eliom_tools.F.html
             ~title:"Bitcoin Exchange"
             ~css:[["css"; "BitcoinExchange.css"]]
             ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
             (body ~a:[a_class ["transparent"]]
             [header_navbar_skeleton ~on_page:`NewAccount user;
              div ~a:[a_class ["margin_top_50px"; "padding_top_50px"]; a_id "registration_fail"]
              [h2 [pcdata user_registration_msg];
              ]
             ]))
          )
