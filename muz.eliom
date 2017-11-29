{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Eliom_parameter
}}

open Db_funs

let user_info =
  Eliom_reference.Volatile.eref ~scope:Eliom_common.default_session_scope ~secure:true
    {username = None;
     email = None;
     verified = None
    }

module Config =
  struct
    (* port access to the website *)
    let port = 8080
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

(* List New Item Service *)
let list_new_item_service =
  Eliom_service.App.service ~path:["new_item"] ~get_params:Eliom_parameter.unit ()

(* Action to write the new story to the db *)
let new_story_action =
  Eliom_service.Http.post_coservice' ~post_params:(string "title" **
                                                   (opt (file "pic")) **
                                                   string "hashtags") ()

(* User page service *)
let user_page_service =
  Eliom_service.Http.service ~path:["u"] ~get_params:(suffix (string "username")) ()

(* Hashtag page service *)
let hashtag_page_service =
  Eliom_service.Http.service ~path:["h"] ~get_params:(suffix (string "hashtag")) ()

(* Service to display a single item for sale *)
let single_item_page_service =
  Eliom_service.Http.service ~path:["item"] ~get_params:(suffix (string "item_id")) ()

(* Form for uploading a picture *)
let pic_form_service =
  Eliom_service.Http.service ~path:["pic_upload"] ~get_params:Eliom_parameter.unit ()

(* Service to save picture *)
let pic_upload_service =
  Eliom_service.Http.post_service ~fallback:main_service ~post_params:(file "pic") ()

(* Service to search available item listings *)
let search_service =
  Eliom_service.Http.post_service ~fallback:main_service ~post_params:(string "search_input") ()

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

let new_item_button =
  div ~a:[a_class ["btn btn-default btn-lg"]; a_id "header_button"]
  [a list_new_item_service [pcdata "List Item "] ()
  ]

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

(* Insert a cat link if the user does not upload a story photo *)
let cat_or_photo so =
  match so with
  | Some s -> s
  | None   -> "https://pbs.twimg.com/profile_images/664169149002874880/z1fmxo00.jpg"

let load_photo_lwt path =
  let open Lwt_unix in
  let open Lwt_io in
  lwt in_chan = open_file ~flags:[O_RDONLY] ~perm:0o640 ~mode:input path in
  let src = read in_chan in
  close in_chan >>
  src

let load_photo path =
  let open UnixLabels in
  let in_chan = open_in_gen [Open_rdonly] 0o640 path in
  let src = input_line in_chan in (* NOTE: Should only be a single line saved *)
  close_in in_chan;
  src

(* Image link a thumbnail to a single item for sale page service *)
let thumbnail_button (i : item) =
  div ~a:[]
  [a single_item_page_service
   [img ~a:[a_style "border-radius: 10px; width: 180px; height: 180px"]
     ~alt:(i.title)
     ~src:(
         let thumb_pic_link = "./static/user_pics/" ^ i.date_time ^ ".txt" in
         Xml.uri_of_string @@ load_photo thumb_pic_link
     )
     ()
   ]
   (string_of_int i.id)
  ]

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

          div ~a:[a_id "optional_section"] [];
          div ~a:[a_style "text-align: center"] [h5 [pcdata "Your email address is optional."]];

          div ~a:[a_class ["form-group"]]
          [div ~a:[a_class ["input-group"]]
           [Raw.span ~a:[a_class ["input-group-addon"]]
            [Raw.span ~a:[a_class ["glyphicon glyphicon-envelope"]] []
            ];
            string_input ~a:[a_class ["form-control"]; a_placeholder "Email Address"]
                         ~input_type:`Text ~name:new_email ()
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
    fun (title, (pic, hashtags)) ->
      [div ~a:[a_style "margin: auto; margin-top: 75px; width: 800px"]
       [div ~a:[a_class ["panel panel-primary"];
                a_style "border: 1px solid #634271; width: 800px; margin: auto;
                         margin-top: 25px; border-radius: 4px"]
        [div ~a:[a_class ["panel-heading"];
                 a_style "background-color: #634271; border: 1px solid #634271; border-radius: 0px"]
         [h3 ~a:[a_class ["panel-title"; "text-center"]] [pcdata "Submit"]
         ];

         div ~a:[a_class ["panel-body"]; a_style "border-radius: 4px; background: whitesmoke"]
         [textarea ~a:[a_class ["form-control"];
                       a_placeholder "Title";
                       a_style "height: 40px"]
                   ~name:title ()
         ];

         div ~a:[a_class ["panel-body"]; a_style "border-radius: 4px; background: whitesmoke"]
         [file_input ~a:[a_id "pic_input"] ~name:pic ()];

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

(* Picture Drag and Drop *)
let drag_drop_area =
  div ~a:[a_id "pic_drop_area"]
  [div ~a:[a_class ["main_pic_drop_div"]; a_id "main_pic_drop"] [pcdata "Main Picture"];
   div ~a:[a_id "vertical_pic_div"]
   [div ~a:[a_id "horizontal_pic_div"]
    [div ~a:[a_id "pic_drop_div_1"; a_class ["pic_drop_div"]] [pcdata "1"];
     div ~a:[a_id "pic_drop_div_2"; a_class ["pic_drop_div"]] [pcdata "2"];
    ];
    div ~a:[a_id "horizontal_pic_div"]
    [div ~a:[a_id "pic_drop_div_3"; a_class ["pic_drop_div"]] [pcdata "3"];
     div ~a:[a_id "pic_drop_div_4"; a_class ["pic_drop_div"]] [pcdata "4"];
    ]
   ];
  ]

(* Store the new item *)
{shared{

  open Deriving_Json
  type new_item_info_args = (string * string) deriving (Json)

}}

(* Store the new item title and description *)
let store_new_item_info ((title : string), (body : string)) =
  lwt write_time = Db_funs.write_new_item_info ~title ~body in
  Lwt.return write_time

let store_photo ((photo_update : string),  (pic_path : string)) =
  let open Lwt_unix in
  let open Lwt_io in
  lwt out_chan =
    open_file ~flags:[O_WRONLY; O_APPEND; O_CREAT] ~perm:0o640 ~mode:output pic_path
  in
  write out_chan photo_update >>
  flush out_chan >>
  close out_chan

{server{

  let store_new_item_info' = server_function Json.t<new_item_info_args> store_new_item_info
  let store_photo' = server_function Json.t<new_item_info_args> store_photo

}}

{client{

  open Dom
  open Dom_html

  (* Maximum size for a single string upload *)
  let max_upload_size = 1_000

  let drag_drop_area_js =
    let pic_drop_area = createDiv document in
    pic_drop_area##id <- Js.string "pic_drop_area";
    pic_drop_area##textContent <- Js.some @@ Js.string "Main Picture";
    pic_drop_area

  (* Split the picure string into separate parts before it is passed to the server *)
  (* this avoids the exception Ocsigen_lib_base.input_is_too_large *)
  let rec buffered_string ?(acc = []) s =
    match Bytes.length s > max_upload_size with
    | false -> List.rev @@ s :: acc
    | true ->
      let hd = String.sub s 0 max_upload_size in
      let remaining_string = String.sub s max_upload_size (String.length s - max_upload_size) in
      buffered_string ~acc:(hd :: acc) remaining_string

  let store_new_photo_js ~pic_name ~pic_string =
    let sl = buffered_string pic_string in
    let pic_path = "./static/user_pics/" ^ pic_name ^ ".txt" in
    window##alert(Js.string ("Writing the picture to " ^ pic_path));
    Lwt_list.map_s (fun s -> %store_photo' (s, pic_path)) sl

  let new_item_form_js () =

    let outer_div = createDiv document in
    outer_div##id <- Js.string "list_item_form_outer_div";

    let list_item_panel = createDiv document in
    list_item_panel##className <- Js.string "panel panel-primary";
    list_item_panel##id <- Js.string "list_item_panel";
    appendChild outer_div list_item_panel;

    let list_item_heading = createDiv document in
    list_item_heading##className <- Js.string "panel-heading";
    list_item_heading##id <- Js.string "list_item_heading";
    appendChild list_item_panel list_item_heading;

    let list_item_title = createH3 document in
    list_item_title##className <- Js.string "panel-title";
    list_item_title##id <- Js.string "list_item_title";
    list_item_title##textContent <- Js.some @@ Js.string "List an Item for Sale";
    appendChild list_item_heading list_item_title;

    let list_item_body_div = createDiv document in
    list_item_body_div##className <- Js.string "panel-body";
    list_item_body_div##id <- Js.string "list_item_body";
    appendChild list_item_panel list_item_body_div;
    let text_area = createTextarea ~name:(Js.string "listing_title") document in
    text_area##className <- Js.string "form-control";
    text_area##id <- Js.string "list_item_title_text";
    text_area##placeholder <- Js.string "Listing Title";
    appendChild list_item_body_div text_area;

    let pic_drop_header = createDiv document in
    pic_drop_header##id <- Js.string "pic_drop_header";
    pic_drop_header##textContent <- Js.some @@ Js.string "Drag and drop pictures";
    appendChild list_item_body_div pic_drop_header;

    appendChild list_item_body_div drag_drop_area_js;

    let description_div = createDiv document in
    description_div##className <- Js.string "panel-body";
    description_div##id <- Js.string "list_item_body";
    appendChild list_item_body_div description_div;

    let description_text_area = createTextarea ~name:(Js.string "item_description") document in
    description_text_area##className <- Js.string "form-control";
    description_text_area##id <- Js.string "list_item_body_text";
    description_text_area##placeholder <- Js.string "Describe your item...";
    appendChild description_div description_text_area;

    let button_div = createDiv document in
    button_div##id <- Js.string "list_item_button_div";
    appendChild list_item_body_div button_div;

    let submit_button = createButton ~_type:(Js.string "Submit") document in
    submit_button##className <- Js.string "btn btn-lg btn-success btn-block";
    submit_button##id <- Js.string "list_item_submit_button";
    submit_button##textContent <- Js.some @@ Js.string "Submit";
    submit_button##onmousedown <- handler (fun (clk : mouseEvent Js.t) ->
       if clk##button = 0
       then (
         let description =
           let (text_area : Dom_html.textAreaElement Js.t Js.opt) =
             Dom_html.CoerceTo.textarea (getElementById "list_item_body_text")
           in
           match Js.Opt.to_option text_area with
           | None -> "No Description Found"
           | Some e -> Js.to_string e##value
         in
         let title =
           let (text_area : Dom_html.textAreaElement Js.t Js.opt) =
             Dom_html.CoerceTo.textarea (getElementById "list_item_title_text")
           in
           match Js.Opt.to_option text_area with
           | None -> "No title Found"
           | Some e -> Js.to_string e##value
         in
         let pic_string =
           try (
             let dropped_pic = CoerceTo.img (getElementById "dropped_pic") in
             match Js.Opt.to_option dropped_pic with
             | None -> "No picture found"
             | Some (p : Dom_html.imageElement Js.t) -> Js.to_string p##src
           )
           with _ -> "No picture found"
         in
         (* TODO: There is a max_length that pic_string can take, get around this. *)
         (*let _ = %store_new_item' (title, pic_string, description) in*)
         (* TODO: Get the file name, then use that to upload the file *)
         let _ =
           window##alert(Js.string "Finding the listing time..");
           lwt (listing_time : string) = %store_new_item_info' (title, description) in
           (* TODO pick back up here and use the listing time to store the image on the server *)
           window##alert(Js.string("listing_time = " ^ listing_time));
           store_new_photo_js ~pic_name:listing_time ~pic_string >>
           Lwt.return_unit
         in
         Js._true
       )
       else Js._true
     );
    appendChild button_div submit_button;

    appendChild document##body outer_div
}}


(* Header Navbar Skeleton *)
let header_navbar_skeleton ?(on_page = `Null) (u : user) =
  let b0 = if on_page = `Main then [] else [main_page_button] in
  let b1 = if on_page = `NewAccount then [] else [new_account_button u] in
  let b2 = if on_page = `Login then [] else [login_logout_button u] in
  let b3 = if on_page = `UserHome then [] else [user_page_button u] in
  let b4 = if on_page = `NewItem then [] else [new_item_button] in
  let search_form =
    Eliom_content.Html5.F.post_form ~service:search_service ~port:Config.port
      (
        fun search_input ->
          [div ~a:[a_class ["form-group"]; a_id "search_field"]
           [div ~a:[a_class ["input-group"]]
            [Raw.span ~a:[a_class ["input-group-addon"]]
             [Raw.span ~a:[a_class ["glyphicon glyphicon-search"]] []];
              string_input ~a:[a_class ["form-control"]; a_placeholder "Search Listings..."]
                ~input_type:`Text ~name:search_input ()
            ]
           ]
          ]
      )
  in
  let search_div = [search_form ()] in
  let btns =
    match on_page with
    | `Main -> b1 @ b2 @ b3 @ b4 @ search_div
    | `NewAccount -> b0 @ b2 @ b3 @ b4 @ search_div
    | `Login -> b0 @ b1 @ b3 @ b4 @ search_div
    | `Logout -> b0 @ b1 @ b2 @ b3 @ b4 @ search_div
    | `NewItem -> b0 @ b1 @ b2 @ b3 @ search_div
    | `UserHome -> b0 @ b2 @ b4 @ search_div
    | `SingleStory -> b0 @ b1 @ b2 @ b3 @ b4 @ search_div
    | `Null -> b0 @ b1 @ b2 @ b3 @ b4 @ search_div
  in
  nav ~a:[a_class ["navbar navbar-fixed-top"]]
    [div ~a:[a_class ["container-fluid"]]
     [div ~a:[a_class ["navbar-header"]; a_style "width: 100%"] btns]]

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

(* Turn a csv string of hashtags into a list of links *)
let hashtags_of_sl sl =
  List.map
    (hashtag_button ~extra_style:"float: left; margin: 10px 5px 10px 5px; background: transparent")
    sl

(* Turn a story into html *)
let html_of_story (u : user) (s : story) =
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

   div ~a:[a_id "story"]
   [p ~a:[a_style "margin: 10px 10px 10px 10px; width: 100%; text-align: justify"]
    [pcdata s.body]
   ];

   div ~a:[a_style "border: 2px solid #333; height: 10px; width: 1200px; margin: 20px auto;
                    background-color: #333; border-radius: 5px"] []
  ]

(* Turn an item into html *)
let html_of_item (u : user) (i : item) =
  div ~a:[a_id "main_item_picture"]
  [h1 ~a:[a_style "margin: 40px auto; witdh: 800px; text-align: center"]
   [pcdata i.title];

   img ~a:[a_style "margin: auto; display: block; max-height: 300px; max-width: 1200px;
                    border-radius: 10px; box-shadow: 5px 5px 5px grey"]
     ~alt:"Cats are really cool"
     ~src:(
         let thumb_pic_link = "./static/user_pics/" ^ i.date_time ^ ".txt" in
         Xml.uri_of_string @@ load_photo thumb_pic_link
     )
   ();

   div ~a:[a_id "author_info"]
   [p ~a:[a_style "float: left; margin: 10px 10px 10px 10px; text-align: left"]
    [pcdata (time_string @@ float_of_string @@ i.date_time)]
   ];

   div ~a:[a_id "item"]
   [p ~a:[a_style "margin: 10px 10px 10px 10px; width: 100%; text-align: justify"]
    [pcdata i.body]
   ];
  ]

(* Turn a list of stories into html *)
let html_of_stories (u : user) stories =
  List.map (html_of_story u) stories

(* Turn an item into an html thumbnail *)
let thumb_of_item (i : item) =
  div ~a:[a_class ["thumbnail"]; a_id "main_pg_thumbnail"]
  [thumbnail_button i]

(* Turn a list of items into a list of thumbnails *)
let thumbs_of_items items =
  List.map (thumb_of_item) items

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

let left_banner ~alt link pic_path =
  div
  [Raw.a ~a:[a_href (Xml.uri_of_string link)]
   [img ~a:[a_id "left_ad_banner"] ~alt ~src:(Xml.uri_of_string pic_path) ()]
  ]

(* Create a iFrame to verify the muz.today for BitClick *)
{client{
   let iframe_div () =
     let js_div = Dom_html.createDiv Dom_html.document in
     let iframe_str =
       "<iframe scrolling=\"no\" style=\"border: 0; width: 468px; height: 60px;\" " ^
       "src=\"//ads.bcsyndication.com/get.php?s=23357\"></iframe>"
     in
     js_div##innerHTML <- (Js.string iframe_str);
     Eliom_content.Html5.Of_dom.of_element @@ Dom_html.element js_div
}}

(*** Register Services ***)

(* Main Page Service *)
let () =
  Muz_app.register
    ~service:main_service
    (fun () () ->
      lwt user = Lwt.return @@ Eliom_reference.Volatile.get user_info in
      lwt new_items = Db_funs.get_recent_items ~n:100 () in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"uz"
           ~css:[["css";"muz.css"]]
           ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
           (body ~a:[a_class ["transparent"]]
            [header_navbar_skeleton ~on_page:`Main user;

             div ~a:[a_id "dark_section"]
             [h1 ~a:[a_id "main_page_header"] [pcdata "muz"]
             ];

             div ~a:[a_class ["row"]; a_id "thumbnails_div"]
               (thumbs_of_items new_items);

             (* Prove that I own the website *)
             (*Html5.C.node {{iframe_div ()}};*)

             (* Raw attempt *)
             (*let module Html5 = Eliom_content.Html5.F in
             let raw_attempt =
               << <iframe src="//ads.bcsyndication.com/get.php?s=23357"></iframe> >>
             in
               raw_attempt*)

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
      (new_username, (new_email, (new_password, verify_new_password))) ->
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
              verified = Some false
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
           verified = Some true
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
             verified = Some true
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
         verified = None
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

{client{

  open Dom
  open Dom_html
  open File

(* TODO:
   (1) Setup the entire form with Js, user can drag and drop files
   (2) Have a button that calls a server function to save the new item
   (3) On Success, redirect with window.location = "www.success-page.com"
   (4) On Failure, window.alert message to user to fix issues
*)


  (* TODO: Restrict file size, restrict file type, *)
  let on_drop_handler ?(border_radius = "") (d : divElement Js.t) =
    handler (fun (mouse_event : dragEvent Js.t) ->
      preventDefault mouse_event;
      let (pictures : fileList Js.t) = mouse_event##dataTransfer##files in
      if pictures##length <> 1
      then window##alert (Js.string "Please only drop one file!")
      else (
        let file_reader = jsnew fileReader () in
        let img = createImg document in

        file_reader##onload <- Dom.handler (fun (e : fileReader progressEvent Js.t) ->
          let (g : Js.js_string Js.t Js.Opt.t) = CoerceTo.string file_reader##result in
          let (s : Js.js_string Js.t) = Js.Opt.get g (fun () -> Js.string "Nothing Here...") in
          img##src <- s;
          img##style##borderRadius <- Js.string border_radius;
          img##id <- Js.string"dropped_pic";
          d##textContent <- Js.Opt.option None;
          d##style##lineHeight <- Js.string "0px";
          d##style##backgroundColor <- Js.string "transparent";
          d##style##border <- Js.string "none";
          appendChild d img;
          Js._true
        );

        let (pic_file_opt : File.file Js.t Js.Opt.t) = pictures##item(0) in
        let (t : File.file Js.t option) = Js.Opt.to_option pic_file_opt in

        match t with
        | None -> window##alert (Js.string "No file found")
        | Some x -> file_reader##readAsDataURL(x);

      );
      Js._false
    )

  (* TODO: Try setting up a new form using Js only,
     then when submit is clicked call the new item action *)

  (* User drops a picture on the div, which is then assigned to the file <input> *)
  let on_drop_handler_2 ?(border_radius = "") (file_input : Dom_html.element Js.t) =
    handler (fun (mouse_event : dragEvent Js.t) ->
      preventDefault mouse_event;
      let (pictures : fileList Js.t) = mouse_event##dataTransfer##files in
      if pictures##length <> 1
      then window##alert (Js.string "Please drop one file at a time!")
      else (
        let file_reader = jsnew fileReader () in
        let (pic_file_opt : File.file Js.t Js.Opt.t) = pictures##item(0) in
        let (dropped_file : File.file Js.t option) = Js.Opt.to_option pic_file_opt in

        match dropped_file with
        | None -> window##alert (Js.string "No file found")
        | Some pic -> (
          file_reader##readAsDataURL(pic);
          let () =
            match Dom_html.tagged file_input with
            | Input i -> (
                match dropped_file with
                | None -> window##alert (Js.string "No dropped_file")
                | Some f -> (
                    Firebug.console##log(f);
                    i##setAttribute(Js.string "value", f##name)
                  )
              )
            | _ -> ()
          in
          ()
        )
      );
      Js._false
    )

  let on_drag_over_handler (d : divElement Js.t) =
    handler (fun (mouse_event : dragEvent Js.t) ->
        preventDefault mouse_event;
        d##style##backgroundColor <- Js.string "yellow";
        Js._false
      )

  let on_drag_leave_handler (d : divElement Js.t) =
    handler (fun (mouse_event : dragEvent Js.t) ->
        d##style##backgroundColor <- Js.string "transparent";
        Js._false
      )

  let setup_hover_over () =
    let main_pic_div = getElementById "pic_drop_area" in
    main_pic_div##ondragover  <- on_drag_over_handler main_pic_div;
    main_pic_div##ondragleave <- on_drag_leave_handler main_pic_div;
    main_pic_div##ondrop      <- on_drop_handler ~border_radius:"8px 8px 8px 8px" main_pic_div

}}

(* New Item Service *)
let () =
  Muz_app.register
    ~service:list_new_item_service
    (fun () () ->
      let user = Eliom_reference.Volatile.get user_info in
      let _ = {unit{new_item_form_js ()}} in
      let _ = {unit{setup_hover_over ()}} in
      Lwt.return
        (Eliom_tools.F.html
          ~title:"List Item for Sale"
          ~css:[["css";"muz.css"]]
          ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
          (body ~a:[a_class ["transparent"]]
           [header_navbar_skeleton ~on_page:`NewItem user
            (*new_item_form ()*)
           ]
          )
        )
    )

let pic_path (u : user) =
  match u.username, u.verified with
  | Some un, Some true ->
      "static/user_pics/" ^ un ^ (string_of_float @@ Unix.time ()) ^ "jpg"
  | _ -> ""

(* Save a thumbnail version of an existing pic - just a compressed version *)
let save_thumbnail pic_path =
  let thumbnail_name = (String.sub pic_path 0 (String.length pic_path - 4)) ^ "_thumbnail.jpg" in
  Lwt_unix.system (
    "convert " ^ pic_path ^ " -strip -gaussian-blur 0.05 -quality 50 " ^
    "-resize 200x200 " ^ thumbnail_name
  )

let save_pic pic pic_path =
  (try Unix.unlink pic_path; with _ -> ());
  Lwt_unix.link (Eliom_request_info.get_tmp_filename pic) pic_path
  >> save_thumbnail pic_path
  |> fun _ -> Lwt.return_unit

(* User Page Service *)
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

(* Single Item Page Service *)
let () =
  Eliom_registration.Html5.register
    ~service:single_item_page_service
    (fun item_id () ->
       let user = Eliom_reference.Volatile.get user_info in
       lwt itm = get_item item_id in
       let item_html, item_id, item_title =
         match itm with
         | Some i -> (html_of_item user i, string_of_int i.id, i.title)
         | None   -> (
             h3 ~a:[a_style "text-align: center"] [pcdata "Item Not Found"],
             "",
             "Item Not Found"
           )
       in
       Lwt.return
         (Eliom_tools.F.html
           ~title:"muz"
           ~css:[["css"; "muz.css"]]
           ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
           (body ~a:[a_class ["transparent"]]
            [header_navbar_skeleton ~on_page:`Null user;
             div ~a:[a_id "center_content"] [item_html];
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

(* Listing Search Service *)
let () =
  Eliom_registration.Html5.register
    ~service:search_service
    (fun () search_input ->
       let user = Eliom_reference.Volatile.get user_info in
      Lwt.return
        (Eliom_tools.F.html
          ~title:"Search Item Listings"
          ~css:[["css";"muz.css"]]
          ~other_head:[bootstrap_cdn_link; font_awesome_cdn_link]
          (body ~a:[a_class ["transparent"]]
             [header_navbar_skeleton user;
              h1 [pcdata ("You searched for = " ^ search_input)];
             ]
          )
        )
    )
