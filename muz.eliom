{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Eliom_parameter
}}

open Db_funs

(* TODO: Show number of thumbs up / down next to a story *)

let user_info =
  Eliom_reference.Volatile.eref ~scope:Eliom_common.default_session_scope ~secure:true
    {username = None;
     email = None;
     verified = None;
     location = {
       country = None;
       state = None;
       city = None;
       hood = None;
       school = None
     }
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
                                                string "verify_new_password" **
                                                string "country" **
                                                string "state" **
                                                string "city" **
                                                string "hood" **
                                                string "school") ()

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
  Eliom_service.Http.post_coservice' ~post_params:(string "title" **
                                                   string "body" **
                                                   (opt (file "pic")) **
                                                    string "hashtags") ()

(* User page service *)
let user_page_service =
  Eliom_service.Http.service ~path:["u"] ~get_params:(suffix (string "username")) ()

(* Hashtag page service *)
let hashtag_page_service =
  Eliom_service.Http.service ~path:["h"] ~get_params:(suffix (string "hashtag")) ()

(* Form for uploading a picture *)
let pic_form_service =
  Eliom_service.Http.service ~path:["pic_upload"] ~get_params:Eliom_parameter.unit ()

(* Service to save picture *)
let pic_upload_service =
  Eliom_service.Http.post_service ~fallback:main_service ~post_params:(file "pic") ()

(* Action to handle a Thumbs Up *)
let thumbs_up_action =
  Eliom_service.Http.post_coservice' ~post_params:(int "id") ()

(* Action to handle a Thumbs Down *)
let thumbs_down_action =
  Eliom_service.Http.post_coservice' ~post_params:(int "id") ()

(*** Page Elements ***)

(* Bootstrap CDN link *)
let bootstrap_cdn_link =
    let cdn_link = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" in
      link ~rel:[`Stylesheet] ~href:(Xml.uri_of_string cdn_link)
        ()

(* FontAwesome CDN link *)
let font_awesome_cdn_link =
    let cdn_link = "//netdna.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css" in
      link ~rel:[`Stylesheet] ~href:(Xml.uri_of_string cdn_link)
        ()

let main_page_button =
  div ~a:[a_class ["btn btn-default btn-lg"]; a_id "header_button"]
  [a main_service [pcdata "Home"] ()
  ]

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

let new_story_button (u : user) =
  match u.verified with
  | Some true ->
    begin
      div ~a:[a_class ["btn btn-default btn-lg"]; a_id "header_button"]
      [a new_story_service [pcdata "Submit a Story"] ()
      ]
    end
  | _ -> div []

let user_page_button (u : user) =
  match u.verified with
  | Some true ->
    begin
      div ~a:[a_class ["btn btn-default btn-lg"]; a_id "header_button"]
      [a user_page_service [pcdata "My Homepage"] (string_of_option u.username)
      ]
    end
  | _ -> div []

let hashtag_button ?(extra_style = "") hashtag =
  div
    ~a:[a_style ("color: #634271 !important; background: transparent; border: none;" ^ extra_style)]
  [a hashtag_page_service [pcdata ("#" ^ hashtag)] hashtag
  ]

let author_button ?(extra_style = "") username =
  div
    ~a:[a_style ("float: left; margin: 10px 10px 10px 10px; text-align: left;
                  color: #634271 !important; background: transparent; font-weight: bold;" ^
                 extra_style)]
  [a user_page_service [pcdata ("@" ^ username)] username
  ]

let thumbs_up_button ?(picked = false) (s : story) =
  let thumb_color =
    match picked with
    | true -> "color: green"
    | false -> "color: #333"
  in
  Eliom_content.Html5.F.post_form ~service:thumbs_up_action ~port:Config.port
  (
    fun id ->
      [
       div ~a:[a_id "thumb"]
       [int_input ~input_type:`Hidden ~name:id ~value:s.id ();
        button ~a:[a_class ["glyphicon glyphicon-thumbs-up"]; a_style thumb_color]
               ~button_type:`Submit []
       ]
      ]
  )

let thumbs_down_button ?(picked = false) (s : story) =
  let thumb_color =
    match picked with
    | true -> "color: red"
    | false -> "color #333"
  in
  Eliom_content.Html5.F.post_form ~service:thumbs_down_action ~port:Config.port
  (
    fun id ->
      [
       div ~a:[a_id "thumb"]
       [int_input ~input_type:`Hidden ~name:id ~value:s.id ();
        button ~a:[a_class ["glyphicon glyphicon-thumbs-down"]; a_style thumb_color]
               ~button_type:`Submit []
       ]
      ]
  )

let new_account_form =
  Eliom_content.Html5.F.post_form ~service:new_acct_db_service ~port:Config.port
  (
    fun (new_username,
         (new_email,
          (new_password,
           (verify_new_password,
            (country,
             (state,
              (city,
               (hood, school)))))))) ->
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

          (* Optional Location Section *)
          div ~a:[a_style "text-align: center"] [pcdata "This section is optional."];
          div ~a:[a_style "text-align: center; margin-bottom: 15px"]
          [pcdata "You do NOT have to provide this information."];

          div ~a:[a_class ["form-group"]]
          [div ~a:[a_class ["input-group"]]
           [Raw.span ~a:[a_class ["input-group-addon"]]
            [Raw.span ~a:[a_class ["glyphicon glyphicon-envelope"]] []
            ];
            string_input ~a:[a_class ["form-control"]; a_placeholder "Email Address"]
                         ~input_type:`Text ~name:new_email ()
           ]
          ];

           div ~a:[a_class ["form-group"]]
           [div ~a:[a_class ["input-group"]]
            [Raw.span ~a:[a_class ["input-group-addon"]]
             [Raw.span ~a:[a_class ["glyphicon glyphicon-globe"]] []
             ];
             string_input ~a:[a_class ["form-control"]; a_placeholder "Country"]
                          ~input_type:`Text ~name:country ()
            ]
           ];

           div ~a:[a_class ["form-group"]]
           [div ~a:[a_class ["input-group"]]
            [Raw.span ~a:[a_class ["input-group-addon"]]
             [Raw.span ~a:[a_class ["glyphicon glyphicon-globe"]] []
             ];
             string_input ~a:[a_class ["form-control"]; a_placeholder "State"]
                          ~input_type:`Text ~name:state ()
            ]
           ];

           div ~a:[a_class ["form-group"]]
           [div ~a:[a_class ["input-group"]]
            [Raw.span ~a:[a_class ["input-group-addon"]]
             [Raw.span ~a:[a_class ["glyphicon glyphicon-globe"]] []
             ];
             string_input ~a:[a_class ["form-control"]; a_placeholder "City"]
                          ~input_type:`Text ~name:city ()
            ]
           ];

           div ~a:[a_class ["form-group"]]
           [div ~a:[a_class ["input-group"]]
            [Raw.span ~a:[a_class ["input-group-addon"]]
             [Raw.span ~a:[a_class ["glyphicon glyphicon-home"]] []
             ];
             string_input ~a:[a_class ["form-control"]; a_placeholder "Hood"]
                          ~input_type:`Text ~name:hood ()
            ]
           ];

           div ~a:[a_class ["form-group"]]
           [div ~a:[a_class ["input-group"]]
            [Raw.span ~a:[a_class ["input-group-addon"]]
             [Raw.span ~a:[a_class ["glyphicon glyphicon-education"]] []
             ];
             string_input ~a:[a_class ["form-control"]; a_placeholder "School"]
                          ~input_type:`Text ~name:school ()
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
    fun (title, (body, (pic, hashtags))) ->
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
                       a_placeholder "Title";
                       a_style "height: 40px"]
                   ~name:title ()
         ];

         (*
         div ~a:[a_class ["panel-body"]; a_style "border-radius: 4px; background: whitesmoke"]
         [textarea ~a:[a_class ["form-control"];
                       a_placeholder "Picture Link";
                       a_style "height: 40px"]
                   ~name:pic_link ()
         ];
         *)

         div ~a:[a_class ["panel-body"]; a_style "border-radius: 4px; background: whitesmoke"]
         [file_input ~a:[a_id "pic_input"] ~name:pic ()];

         div ~a:[a_class ["panel-body"]; a_style "border-radius: 4px; background: whitesmoke"]
         [textarea ~a:[a_class ["form-control"];
                       a_placeholder "Body (10 - 10,000 characters)";
                       a_style "height: 200px"]
                   ~name:body ()
         ];

         div ~a:[a_class ["panel-body"]; a_style "border-radius: 4px; background: whitesmoke"]
         [textarea ~a:[a_class ["form-control"];
                       a_placeholder "Hashtags";
                       a_style "height: 40px"]
                   ~name:hashtags ()
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
  let b0 = if on_page = `Main then [] else [main_page_button] in
  let b1 = if on_page = `NewAccount then [] else [new_account_button u] in
  let b2 = if on_page = `Login then [] else [login_logout_button u] in
  let b3 = if on_page = `NewStory then [] else [new_story_button u] in
  let b4 = if on_page = `UserHome then [] else [user_page_button u] in
  let btns =
    match on_page with
    | `Main -> b1 @ b2 @ b3 @ b4
    | `NewAccount -> b0 @ b2 @ b3 @ b4
    | `Login -> b0 @ b1 @ b3 @ b4
    | `Logout -> b0 @ b1 @ b2 @ b3 @ b4
    | `NewStory -> b0 @ b1 @ b2 @ b4
    | `UserHome -> b0 @ b2 @ b3
    | `Null -> b0 @ b1 @ b2 @ b3 @ b4
  in
  nav ~a:[a_class ["navbar navbar-fixed-top"]; a_style "background-color: #333;"]
    [div ~a:[a_class ["container-fluid"]]
     [div ~a:[a_class ["navbar-header"]; a_style "width: 100%"] btns]]

(* Insert a cat link if the user does not upload a story photo *)
let cat_or_photo so =
  match so with
  | Some s -> s
  | None   -> "https://pbs.twimg.com/profile_images/664169149002874880/z1fmxo00.jpg"

(* Convert a float into a formatted string *)
let time_string f =
  let open Unix in
  let t = Unix.gmtime f in
  let month = string_of_int (t.tm_mon + 1) in
  let day = string_of_int t.tm_mday in
  let year = string_of_int (1900 + t.tm_year) in
  let hour, am_pm =
    if t.tm_hour <= 11
    then (string_of_int t.tm_hour, "am")
    else (string_of_int (t.tm_hour - 12), "pm")
  in
  let min = string_of_int t.tm_min in
  let sec = string_of_int t.tm_sec in
  (month ^ "/" ^ day ^ "/" ^ year ^ "   " ^ hour ^ ":" ^ min ^ ":" ^ sec ^ " " ^ am_pm)

(* Handmade Core.Core_string.split_on_chars b/c core breaks in Ocsigen *)
let split_string_on in_string ~on =
  (* Split the string into a list of its individual characters, as strings not characters *)
  let list_of_string s =
    let rec build_string_list in_string out_string_list =
      match String.length in_string with
      | 0 -> List.rev out_string_list
      | _ -> build_string_list (String.sub in_string 1 ((String.length in_string) -1))
                               ((String.sub in_string 0 1) :: out_string_list)
    in
    build_string_list s []
  in
  (* Then concatenate and split into another list based on the chars chosen *)
  let build_final_string_list sl ~split_on =
    let rec f curr_string curr_list remaining =
      match List.length remaining with
      | 0 -> List.rev (if curr_string = "" then curr_list else (curr_string :: curr_list))
      | _ -> if List.mem (List.hd remaining) split_on
             then f "" (curr_string :: curr_list) (List.tl remaining)
             else f (curr_string ^ (List.hd remaining)) curr_list (List.tl remaining)
    in
    f "" [] sl
  in
  build_final_string_list (list_of_string in_string) ~split_on:on

(* Turn a csv string of hashtags into a list of links *)
let hashtags_of_sl sl =
  List.map
    (hashtag_button ~extra_style:"float: left; margin: 10px 5px 10px 5px; background: transparent")
    sl

(* Turn a story into html *)
let html_of_story (u : user) (s : story) =
  let t_up, t_down =
    match u.verified, u.username with
    | Some true, Some un ->
      (
        List.mem un (get_thumbs ~up_down:`Up (string_of_int s.id)),
        List.mem un (get_thumbs ~up_down:`Down (string_of_int s.id))
      )
    | _, _ -> (false, false)
  in
  div
  [h1 ~a:[a_style "margin: 40px auto; witdh: 800px; text-align: center"]
   [pcdata s.title];

   img ~a:[a_style "margin: auto; display: block; max-height: 300px; max-width: 1200px;
                    border-radius: 10px; box-shadow: 5px 5px 5px grey"]
     ~alt:"Cats are really cool"
     ~src:(
       match s.pic_link with
       | Some pl ->
         let path_list = split_string_on pl ~on:["/"] |> List.tl in
          make_uri ~service:(Eliom_service.static_dir ()) path_list
       | _ -> (Xml.uri_of_string (cat_or_photo None))
     )
   ();

   div ~a:[a_id "author_info"]
   [author_button s.author;
    p ~a:[a_style "float: left; margin: 10px 10px 10px 10px; text-align: left"]
    [pcdata (time_string @@ float_of_string @@ s.date_time)]
   ];

   div ~a:[a_id "story_hashtags"] (hashtags_of_sl s.hashtags);

   div ~a:[a_id "thumbs"]
   [thumbs_up_button ~picked:t_up s (); thumbs_down_button ~picked:t_down s ()];

   div ~a:[a_id "story"]
   [p ~a:[a_style "margin: 10px 10px 10px 10px; width: 1200px; text-align: justify"]
    [pcdata s.body]
   ];

   div ~a:[a_style "border: 2px solid #333; height: 10px; width: 1200px; margin: 20px auto;
                    background-color: #333; border-radius: 5px"] []
  ]

(* Turn a list of stories into html *)
let html_of_stories (u : user) stories =
  List.map (html_of_story u) stories

(* Turn a story into an html thumbnail *)
let thumb_of_story (s : story) =
  let safe_title =
    if String.length s.title <= 20
    then s.title
    else
      begin
        try (String.sub s.title 0 (min (String.length s.title) 20)) ^ "..." with
        | Invalid_argument "String.sub / Bytes.sub" -> "Untitled..."
        | _ -> "Title Error..."
      end
  in
  let safe_body =
    if String.length s.body <= 375
    then s.body
    else
      begin
        try (String.sub s.body 0 (min (String.length s.body) 375)) ^ "..." with
        | Invalid_argument "String.sub / Bytes.sub" -> "Nothin here..."
        | _ -> "Something went wrong..."
      end
  in
  let style_string =
    "width: 300px; float: left; height: 600px; margin-top: 10px; margin-bottom: 10px;" ^
    "margin-left: 25px; border-radius: 10px; box-shadow: 5px 5px 5px grey"
  in
  div ~a:[a_class ["thumbnail"]; a_style style_string]
  [img ~a:[a_style "border-radius: 10px; margin-top: 13px; max-width: 260px; max-height: 300px"]
     ~alt:(s.title)
     ~src:(
       match s.pic_link with
       | Some pl ->
         let path_list = split_string_on pl ~on:["/"] |> List.tl in
          make_uri ~service:(Eliom_service.static_dir ()) path_list
       | _ -> (Xml.uri_of_string (cat_or_photo None))
     )
     ();
   div ~a:[a_class ["caption"]]
   [h3 [pcdata safe_title];
    p [pcdata safe_body]
   ]
  ]

(* Turn a list of stories into a list of thumbnails *)
let thumbs_of_stories stories =
  List.map (thumb_of_story) stories

(* Limit the length of a string and pad with "..." *)
let safe_string ~max_len s =
  if String.length s <= max_len
  then s
  else
    begin
      try (String.sub s 0 (min (String.length s) max_len)) ^ "..." with
      | Invalid_argument "String.sub / Bytes.sub" -> "Whoops..."
      | _ -> "Uh Oh..."
    end

(* Most popular hashtags of the last 24 hours - Limit to 10 max *)
let most_pop_hashtags () =
  lwt pop_htgs = Db_funs.get_recent_hashtags ~n:10 () in
  match List.length pop_htgs with
  | 0 -> Lwt.return []
  | _ -> Lwt.return @@ List.map (fun s -> li [hashtag_button s]) pop_htgs

let most_pop_hashtag_trs () =
  lwt pop_htgs = Db_funs.get_recent_hashtags ~n:10 () in
  match List.length pop_htgs with
  | 0 -> Lwt.return []
  | _ -> Lwt.return @@ List.map (fun s -> tr[td [hashtag_button s]]) pop_htgs

let html_of_categories sl =
  ul ~a:[a_class ["nav nav-pills nav-stacked"];
         a_style "width: 200px; height: 400px; border: 2px solid black;
                  float: left; margin-left: 50px; border-radius: 15px; text-align: center;
                  padding: 10px; box-shadow: 5px 5px 5px grey; background: #333"]
    sl

(* Pad out remaining rows in a table if necessary *)
let rec top_n_rows ~n l_in l_out =
  match l_in, l_out with
  | [], _ ->
      if List.length l_out < n
      then
        top_n_rows ~n []
        (l_out @ [tr ~a:[a_style "height: 30px"] [td [pcdata ""]]])
      else l_out
  | hd :: tl, _ ->
      if List.length l_out < n
      then top_n_rows ~n tl (l_out @ [hd])
      else l_out

let top_hashtags_table () =
  lwt pop_htgs = Db_funs.get_recent_hashtags ~n:10 () in
  let pop_hashtags = List.map (fun s -> hashtag_button s) pop_htgs in
  let table_title =
    tr ~a:[a_id "hashtag_table_title"]
    [td ~a:[a_style "background: #333; border-radius: 10px 0px;"]
     [pcdata "Top Hashtags"]
    ]
  in
  let hashtag_trs =
    List.map (fun hashtag -> tr ~a:[a_style "height: 30px"] [td [hashtag]]) pop_hashtags
  in
  let hashtag_tbl = table ~a:[a_class ["table"]] (top_n_rows ~n:10 hashtag_trs [table_title]) in
  Lwt.return @@
  div ~a:[a_id "hashtag_table"]
  [div ~a:[a_class ["text-center"]] [hashtag_tbl]
  ]

(*** Register Services ***)

(* Main Page Service *)
let () =
  Muz_app.register
    ~service:main_service
    (fun () () ->
      lwt user = Lwt.return @@ Eliom_reference.Volatile.get user_info in
      lwt newest_story = Db_funs.get_newest_story () in
      lwt new_stories = Db_funs.get_recent_stories ~n:3 () in
      lwt pop_hashtags = most_pop_hashtags () in
      lwt top_htgs_tbl = top_hashtags_table () in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"muz"
           ~css:[["css";"muz.css"]]
           ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
           (body ~a:[a_class ["transparent"]]
            [header_navbar_skeleton ~on_page:`Main user;

             div ~a:[a_id "dark_section"]
             [div [top_htgs_tbl];
              h1 ~a:[a_id "main_page_header"] [pcdata "muz"];
              h3 ~a:[a_style "width: 800px; color: #FFFFFF; font-style: italic; margin: auto;
                              text-align: center"]
              [pcdata ("News right meow!")];
             ];

             div
             [h1 ~a:[a_style "margin: 40px auto 10px; text-align: center; width: 1200px;
                              background: white; border-radius: 10px; height: 75px;
                              font-size: 45px; box-shadow: 5px 5px 5px grey; line-height: 70px"]
              [pcdata (safe_string ~max_len:42 newest_story.title)];

              img ~a:[a_style "margin: auto; display: block; max-height: 300px; max-width: 1200px;
                               border-radius: 10px; box-shadow: 5px 5px 5px grey"]
                  ~alt:"Main Story Picture"
                  ~src:(
                    match newest_story.pic_link with
                    | Some pl ->
                        let path_list = split_string_on pl ~on:["/"] |> List.tl in
                        make_uri ~service:(Eliom_service.static_dir ()) path_list
                    | _ -> (Xml.uri_of_string (cat_or_photo None))) ();

              p ~a:[a_style "margin: 20px auto 40px; text-align: justify; width: 1200px;
                             background: white; border-radius: 10px; font-size: 15px;
                             box-shadow: 5px 5px 5px grey; padding: 10px"]
              [pcdata (safe_string ~max_len:1000 newest_story.body)]
             ];

             div ~a:[a_class ["row"]; a_style "width: 1000px; height: 600px; margin: auto"]
             (thumbs_of_stories new_stories)
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
    (fun ()
      (new_username,
       (new_email,
        (new_password,
         (verify_new_password,
          (country,
           (state,
            (city,
             (hood, school)))))))) ->
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
              email = if new_email <> "" then Some new_email else None;
              verified = Some false;
              location = {
                country  = if country <> "" then Some country else None;
                state = if state <> "" then Some state else None;
                city = if city <> "" then Some city else None;
                hood = if hood <> "" then Some hood else None;
                school = if school <> "" then Some school else None
              }
            }
            in
            let msg =
              try Db_funs.write_new_user new_user new_password
              with Failure _ -> Lwt.return "Oops... Error, please notify Muz. Sorry about that."
            in
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
      if (not username_taken) && (not email_taken) && password_verified && pwd_complexity
      then begin
        Eliom_reference.Volatile.set user_info
          {username = Some new_username;
           email = if new_email <> "" then Some new_email else None;
           verified = Some true;
           location = {
             country  = if country <> "" then Some country else None;
             state = if state <> "" then Some state else None;
             city = if city <> "" then Some city else None;
             hood = if hood <> "" then Some hood else None;
             school = if school <> "" then Some school else None
           }
          };
        let user = Eliom_reference.Volatile.get user_info in
        Lwt.return
          (Eliom_tools.F.html
             ~title:"muz"
             ~css:[["css"; "muz.css"]]
             ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
             (body ~a:[a_class ["transparent"]]
             [header_navbar_skeleton ~on_page:`NewAccount user;
              div ~a:[a_class ["margin_top_50px"; "padding_top_50px"]]
              [h3 ~a:[a_style "margin: auto auto 20px; text-align: center"]
               [pcdata ("Thanks for registering! Enjoy your muz!")];
              ];
             ]))
      end
      else
        let user = Eliom_reference.Volatile.get user_info in
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
            {username = Some username;
             email = None;
             verified = Some true;
             location = Db_funs.get_user_location_info username
            };
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
        {username = None;
         email = None;
         verified = None;
         location = {
           country = None;
           state = None;
           city = None;
           hood = None;
           school = None
         }
        }
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
           [header_navbar_skeleton ~on_page:`NewStory user;
            (
              match user.verified with
              | Some true -> new_story_form ()
              | _ ->
                  h1 ~a:[a_style "margin-top: 100px; text-align: center"]
                  [pcdata "ERROR: You must be logged in to submit a new story."]
            )
           ])))

let pic_path (u : user) =
  match u.username, u.verified with
  | Some un, Some true -> "static/user_pics/" ^ un ^ (string_of_float @@ Unix.time ()) ^ "jpg"
  | _ -> ""

let save_pic pic pic_path =
  (try Unix.unlink pic_path; with _ -> ());
  Lwt_unix.link (Eliom_request_info.get_tmp_filename pic) pic_path

(* Write the new story to the database *)
let () =
  Eliom_registration.Action.register
  ~options:`Reload
  ~service:new_story_action
  (fun () (title, (body, (pic, hashtags))) ->
    ignore {unit{Dom_html.window##alert (Js.string "Test")}};
    (* TODO: Give success/fail message for the contact message *)
    (****** TODO: Why do these popups not work?!?! ******)
     (*lwt () = Lwt_unix.sleep 3.0 in*) (* Throttle *)
     (* TODO: Do a length check for the title also *)
     (* TODO: Users can currently submit a new story without being logged in *)
    let user = Eliom_reference.Volatile.get user_info in
    let long_enough = (Lwt_bytes.length @@ Lwt_bytes.of_string body) >= 10 in
    let short_enough = (Lwt_bytes.length @@ Lwt_bytes.of_string body) <= 10_000 in
    let pp =
      match pic with
      | Some _ -> pic_path user
      | _ -> ""
    in
    lwt () =
      match user.username, user.verified, pic with
        | Some un, Some true, Some p -> save_pic p pp
        | _ -> Lwt.return ()
    in
    match long_enough, short_enough with
    | true, true ->
        begin
          ignore
            {unit{
              Dom_html.window##alert (Js.string "Thanks for the submission!")
              }};
          let pl = match pic with
            | Some p -> Some pp
            | _ -> None
          in
          Db_funs.write_new_story user ~title ~body ~pic_link:pl ~hashtags
        end
    | false, _ ->
        begin
        Lwt.return @@ ignore
          {unit{
            Dom_html.window##alert (Js.string "ERROR: Body must be at least 10 characters.")
            }};
        Lwt.return ()
        end
    | _, false ->
        Lwt.return @@ ignore
          {unit{
            Dom_html.window##alert (Js.string "ERROR: Body must be less than 10,000 characters.")
            }};
        Lwt.return ();
  )

(* User Page Service *)
(* TODO: Add an about me section here *)
let () =
  Eliom_registration.Html5.register
    ~service:user_page_service
    (fun username () ->
       let user = Eliom_reference.Volatile.get user_info in
       let all_stories = List.rev @@ Db_funs.get_all_stories username in
       Lwt.return
         (Eliom_tools.F.html
           ~title:("muz/u/" ^ username)
           ~css:[["css"; "muz.css"]]
           ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
           (body ~a:[a_class ["transparent"]]
            [header_navbar_skeleton ~on_page:`UserHome user;
             h1 ~a:[a_style "margin-top: 100px; text-align: center"]
             [pcdata (username ^ "/home")];
             div (html_of_stories user all_stories)
            ]
           )
         )
    )

(* Hashtag Page Service *)
let () =
  Eliom_registration.Html5.register
    ~service:hashtag_page_service
    (fun hashtag () ->
       let user = Eliom_reference.Volatile.get user_info in
       let tagged_stories = Db_funs.get_stories_by_hashtag hashtag |> (html_of_stories user) in
       Lwt.return
         (Eliom_tools.F.html
           ~title:("muz/h/" ^ hashtag)
           ~css:[["css"; "muz.css"]]
           ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
           (body ~a:[a_class ["transparent"]]
            [header_navbar_skeleton user;
             h1 ~a:[a_style "margin-top: 100px; text-align: center"]
             [pcdata ("hashtag = " ^ hashtag)];

             div (tagged_stories)
            ]
           )
         )
    )

(* Picture upload form *)
let pic_upload_form =
  Eliom_content.Html5.F.post_form ~service:pic_upload_service ~port:Config.port
    (
      fun pic ->
        [
          div ~a:[a_id "pic_input_div"] [file_input ~a:[a_id "pic_input"] ~name:pic ()];

          div ~a:[a_id ""]
          [button ~a:[a_class ["btn btn-lg btn-success btn-block"];
                      a_style "width: 150px; margin: auto; background-color: #634271;
                               border-color: #634271; font-size: 16px"]
                  ~button_type:`Submit [pcdata "Submit"]
            ];
        ]
    )

(* Pic Form Service *)
let () =
  Eliom_registration.Html5.register
    ~service:pic_form_service
    (fun () () ->
      let user = Eliom_reference.Volatile.get user_info in
      Lwt.return
        (Eliom_tools.F.html
          ~title:"Pic Form Service"
          ~css:[["css";"muz.css"]]
          ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
          (body ~a:[a_class ["transparent"]]
           [header_navbar_skeleton user;
            match user.verified with
            | Some true ->
              (
                div ~a:[a_id "pic_upload_form"]
                [div ~a:[a_id "pic_upload_header"]
                 [h1 ~a:[a_id "pic_upload_text"] [pcdata "Upload a picture"]];
                 div ~a:[a_id "pic_upload_body"] [pic_upload_form ()]
                ]
              )
            | _ -> h1 ~a:[a_style "margin-top: 100px; text-align: center;"]
                   [pcdata "Error: Must be logged in to upload photos!"]
           ])))

(* Pic Upload Service *)
let () =
  Eliom_registration.Html5.register
    ~service:pic_upload_service
    (fun () pic ->
       let user = Eliom_reference.Volatile.get user_info in
       let pp = pic_path user in
       lwt () =
         match user.username, user.verified with
           | Some un, Some true -> save_pic pic pp
           | _ -> Lwt.return ()
       in
      Lwt.return
        (Eliom_tools.F.html
          ~title:"Pic Upload Service"
          ~css:[["css";"muz.css"]]
          ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
          (body ~a:[a_class ["transparent"]]
             [header_navbar_skeleton user;
            h1 [pcdata ("Pic saved in: " ^ pp)];
           ])))

(* TODO: When the thumbs actions are updated. The page should re-load back to the same position *)

(* Thumbs Up Action *)
let () =
  Eliom_registration.Action.register
    ~options:`Reload
    ~service:thumbs_up_action
    (fun () id ->
      let user = Eliom_reference.Volatile.get user_info in
      let t_ups = Db_funs.get_thumbs ~up_down:`Up (string_of_int id) in
      let t_downs = Db_funs.get_thumbs ~up_down:`Down (string_of_int id) in
      match user.verified, user.username with
      | Some true, Some un ->
        Lwt.return @@ Db_funs.write_thumbs_action ~up_down:`Up ~id:(string_of_int id) un
      | _ -> Lwt.return ()
    )

(* Thumbs Down Action *)
let () =
  Eliom_registration.Action.register
    ~options:`Reload
    ~service:thumbs_down_action
    (fun () id ->
      let user = Eliom_reference.Volatile.get user_info in
      match user.verified, user.username with
      | Some true, Some un ->
          Lwt.return @@ Db_funs.write_thumbs_action ~up_down:`Down ~id:(string_of_int id) un
      | _ -> Lwt.return ()
    )
