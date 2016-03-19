{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Eliom_parameter
}}

open Db_funs

let user_info =
  Eliom_reference.Volatile.eref ~scope:Eliom_common.default_session_scope ~secure:true
    {username = None; email = None; verified = None}

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

(* Main Page Service *)
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

(* Login Page Service *)
let login_service =
  Eliom_service.Http.service ~path:["login"] ~get_params:Eliom_parameter.unit ()

(* Verify the users login details and set the session data if username and password are verified *)
let login_verify_service =
  Eliom_service.Http.post_service ~fallback:login_service
                                  ~post_params:(string "username" ** string "password") ()

(* Logout Page Service *)
let logout_service =
  Eliom_service.Http.service ~path:["logout"] ~get_params:Eliom_parameter.unit ()

(* New Story Service *)
let new_story_service =
  Eliom_service.Http.service ~path:["new_story"] ~get_params:Eliom_parameter.unit ()

(* Action to write the new story to the db *)
let new_story_action =
  Eliom_service.Http.post_coservice' ~post_params:(string "title" ** string "body") ()

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

let new_account_button (u : user) =
  match u.verified with
  | Some true -> div []
  | _ ->
      div ~a:[a_class ["btn btn-default btn-lg"]; a_id "header_button"]
      [a new_account_service [pcdata "Register New Account"] ()
      ]

let login_button =
  div ~a:[a_class ["btn btn-default btn-lg"]; a_id "header_button"]
  [a login_service [pcdata "Login"] ()
  ]

let logout_button =
  div ~a:[a_class ["btn btn-default btn-lg"]; a_id "header_button"]
  [a logout_service [pcdata "Logout"] ()
  ]

let login_logout_button (u : user) =
  match u.verified with
  | Some true -> logout_button
  | _ -> login_button

let new_account_form =
  Eliom_content.Html5.F.post_form ~service:new_acct_db_service ~port:Config.port
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

let login_form =
  Eliom_content.Html5.F.post_form ~service:login_verify_service ~port:Config.port
  (
    fun (username, password) ->
      [div ~a:[a_id "login_form"]
       [div ~a:[a_class ["panel panel-primary"];
                a_style "border: 1px solid #634271; width: 400px; margin: auto; border-radius: 4px"]
        [div ~a:[a_class ["panel-heading"];
                 a_style "background-color: #634271; border: 1px solid #634271; border-radius: 0px"]
         [h3 ~a:[a_class ["panel-title"; "text-center"]] [pcdata "Login"]
         ];
         div ~a:[a_class ["panel-body"]; a_style "border-radius: 4px; background: whitesmoke"]
         [div ~a:[a_class ["form-group"]]
          [string_input ~a:[a_class ["form-control"]; a_placeholder "Username"]
                        ~input_type:`Text ~name:username ()
          ];
          div ~a:[a_class ["form-group"]]
          [string_input ~a:[a_class ["form-control"]; a_placeholder "Password"]
                        ~input_type:`Password ~name:password ()
          ];
          button ~a:[a_class ["btn btn-lg btn-success btn-block"];
                     a_style "width: 150px; margin: auto; background-color: #634271;
                              border-color: #634271; font-size: 16px"]
                 ~button_type:`Submit [pcdata "Login"]
         ]
        ]
       ]
      ]
  )

(* New Story form *)
let new_story_form =
  Eliom_content.Html5.F.post_form ~service:new_story_action ~port:Config.port
  (
    fun (title, body) ->
      [div ~a:[a_style "margin: auto; margin-top: 75px; width: 800px"]
       [div ~a:[a_class ["panel panel-primary"];
                a_style "border: 1px solid #634271; width: 800px; margin: auto;
                         margin-top: 25px; border-radius: 4px"]
        [div ~a:[a_class ["panel-heading"];
                 a_style "background-color: #634271; border: 1px solid #634271; border-radius: 0px"]
         [h3 ~a:[a_class ["panel-title"; "text-center"]] [pcdata "Submit a New Story"]
         ];
         div ~a:[a_class ["panel-body"]; a_style "border-radius: 4px; background: whitesmoke"]
         [textarea ~a:[a_class ["form-control"];
                       a_placeholder "Type Your Story Title Here";
                       a_style "height: 40px"]
                   ~name:title ()
         ];
         div ~a:[a_class ["panel-body"]; a_style "border-radius: 4px; background: whitesmoke"]
         [textarea ~a:[a_class ["form-control"];
                       a_placeholder "Type Your Story Body Here";
                       a_style "height: 200px"]
                   ~name:body ()
         ];
         div ~a:[a_style "background-color: whitesmoke; padding-bottom: 15px; border-radius: 4px"]
         [button ~a:[a_class ["btn btn-lg btn-success btn-block"];
                     a_style "width: 150px; margin: auto; background-color: #634271;
                              border-color: #634271; font-size: 16px"]
                 ~button_type:`Submit [pcdata "Submit"]
         ]
        ]
       ]
      ]
  )

(* Header Navbar Skeleton *)
let header_navbar_skeleton ?(on_page = `Null) (u : user) =
  let b1 = if on_page = `NewAccount then [] else [new_account_button u] in
  let b2 = if on_page = `Login then [] else [login_logout_button u] in
  let btns =
    match on_page with
    | `NewAccount -> b2
    | `Login -> b1
    | `Logout -> b1 @ b2
    | `Null -> b1 @ b2
  in
  nav ~a:[a_class ["navbar navbar-fixed-top"]; a_style "background-color: #333;"]
    [div ~a:[a_class ["container-fluid"]] [div ~a:[a_class ["navbar-header"]] btns]]

(*** Register Services ***)

(* Main Page Service *)
let () =
  Muz_app.register
    ~service:main_service
    (fun () () ->
      lwt user = Lwt.return @@ Eliom_reference.Volatile.get user_info in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"muz"
           ~css:[["css";"muz.css"]]
           ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
           (body ~a:[a_class ["transparent"]]
            [header_navbar_skeleton user;
             div ~a:[a_id "dark_section"]
             [h1 ~a:[a_id "main_page_header"] [pcdata "muz"];
              h3 ~a:[a_style "width: 800px; color: #FFFFFF; font-style: italic; margin: auto;\
                              text-align: center"]
              [pcdata ("News right meow")];
             ];
             (* TODO: Put the most recent story here when the page is loaded *)
            ]
           )
        )
    )

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
            let new_user = {
              username = Some new_username;
              email = Some new_email;
              verified = Some false
            }
            in
            let msg = Db_funs.write_new_user new_user new_password in
            (* It is ok to force verified=true since it is only for pub addr creation *)
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
                [h1 [pcdata "A Button should go here..."]]
               ];
              ]
             ]))
      end
      else
        Lwt.return
          (Eliom_tools.F.html
             ~title:"muz"
             ~css:[["css"; "muz.css"]]
             ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
             (body ~a:[a_class ["transparent"]]
             [header_navbar_skeleton ~on_page:`NewAccount user;
              div ~a:[a_class ["margin_top_50px"; "padding_top_50px"]; a_id "registration_fail"]
              [h2 [pcdata user_registration_msg];
              ]
             ]))
          )

(* Login Service *)
let () =
  Eliom_registration.Html5.register
    ~service:login_service
    (fun () () ->
      (* Kick off the thread *)
      Lwt.return @@ Eliom_reference.Volatile.get user_info
      >>= fun user ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"Login"
           ~css:[["css"; "muz.css"]]
           ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
           (body ~a:[a_class ["transparent"]]
              [header_navbar_skeleton ~on_page:`Login user;
               login_form ()
              ])))

(* Verify the users login data and set the session data if the verification passes *)
let () =
  Eliom_registration.Redirection.register
    ~service:login_verify_service
    (fun () (username, password) ->
      (* Verify the user *)
      Db_funs.verify_login username password
      >>= fun b ->
      if b
      then
        begin
          Eliom_reference.Volatile.set user_info
            {username = Some username; email = None; verified = Some true};
          Lwt.return main_service
        end
      else
        let () = Lwt.async (fun () -> Lwt_io.print "\n\nLOGIN FAILED!") in
        Lwt.return logout_service
    )

(* Logout Service *)
let () =
  Eliom_registration.Html5.register
    ~service:logout_service
    (fun () () ->
      (* Kick off the thread *)
      Lwt.return @@ Eliom_reference.Volatile.set user_info
        {username = None; email = None; verified = None}
      >>= fun () -> Lwt.return @@ Eliom_reference.Volatile.get user_info
      >>= fun user ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"Logout"
           ~css:[["css"; "muz.css"]]
           ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
           (body ~a:[a_class ["transparent"]]
           [header_navbar_skeleton ~on_page:`Logout user;
            div ~a:[a_class ["container"; "margin_top_50px"; "padding_top_50px"]]
            [h2 [pcdata "Logout Successful"];
            ]
           ])))

(* New Story Service *)
let () =
  Eliom_registration.Html5.register
    ~service:new_story_service
    (fun () () ->
      let user = Eliom_reference.Volatile.get user_info in
      Lwt.return
        (Eliom_tools.F.html
          ~title:"New Story"
          ~css:[["css";"muz.css"]]
          ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
          (body ~a:[a_class ["transparent"]]
           [header_navbar_skeleton user;
            new_story_form ()
           ])))

(* Write the new story to the database *)
let () =
  Eliom_registration.Action.register
  ~options:`Reload
  ~service:new_story_action
  (fun () (title, body) ->
    (* TODO: Give success/fail message for the contact message *)
    (* TODO: Why do these popups not work?!?! *)
     lwt () = Lwt_unix.sleep 3.0 in (* Throttle *)
    (* TODO: Do a length check for the title also *)
    let long_enough = (Lwt_bytes.length @@ Lwt_bytes.of_string body) >= 10 in
    let short_enough = (Lwt_bytes.length @@ Lwt_bytes.of_string body) <= 10_000 in
    match long_enough, short_enough with
    | true, true ->
        begin
          ignore
            {unit{
              Dom_html.window##alert
                (Js.string ("Thanks for the submission! Notify your firends with this link."))
            }};
          (*Lwt.return @@ Db_funs.write_anon_msg anon_msg*)(* TODO: Write story here *)
          Lwt.return ()
        end
    | false, _ ->
        begin
        ignore
          {unit{
            Dom_html.window##alert
              (Js.string ("ERROR: Your message must be at least 10 characters."))
          }};
        Lwt_io.print "ERROR: Your message must be at least 10 characters."
        end
    | _, false ->
        Lwt.return @@ ignore
          {unit{
            Dom_html.window##alert
              (Js.string ("ERROR: Your message must be less than 10,000 characters."))
          }}
  )

