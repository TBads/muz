(* MySql database functions *)

open Mysql

let (>>=) = Lwt.bind

(* TODO Need query to get a users location from the db *)
(* Location type *)
type location = {
  country : string option;
  state : string option;
  city : string option;
  hood : string option;
  school : string option
}

(* User type *)
type user = {
  username : string option;
  email    : string option;
  verified : bool option
}

(* TODO: Add a gps cooridate type and a gps location to the story post *)

(* Story type *)
type story = {
  id        : int;
  title     : string;
  body      : string;
  author    : string;
  pic_link  : string option;
  date_time : string;
  hashtags  : string list;
}

(* Database *)
let user_db = {
  dbhost = None;
  dbname = Some "muz";
  dbport = Some 3306;
  dbpwd = Some "HPMpRjbvWMe49A95xHsFhRyw";
  dbuser = Some "btc_admin_4A3f8E";
  dbsocket = None
}

module Config =
  struct
    (* Number of iterations for password encryption *)
    let pwd_iter_count = 12
  end

let string_of_option so =
  match so with
  | Some s -> s
  | None -> ""

let pic_link_of_option s =
  match s with
  | "" -> None
  | _ -> Some s

let sll_of_res res =
  Mysql.map res (fun a -> Array.to_list a)
  |> List.map (List.map string_of_option)

let sl_of_csv s =
  Str.split (Str.regexp "[,]") s

let story_of_result sl = {
  id = int_of_string @@ List.nth sl 0;
  title = List.nth sl 2;
  body = List.nth sl 3;
  author = List.nth sl 1;
  pic_link = pic_link_of_option @@ List.nth sl 4;
  date_time = List.nth sl 5;
  hashtags = sl_of_csv (List.nth sl 6)
}

let location_of_result sl = {
  country = if List.nth sl 0 = "" then None else Some (List.nth sl 0);
  state = if List.nth sl 1 = "" then None else Some (List.nth sl 1);
  city = if List.nth sl 2 = "" then None else Some (List.nth sl 2);
  hood = if List.nth sl 3 = "" then None else Some (List.nth sl 3);
  school = if List.nth sl 4 = "" then None else Some (List.nth sl 4)
}

(* Check if the username already exists in database *)
let username_exists new_username =
  let conn = connect user_db in
  let sql_stmt =
    "SELECT username FROM muz.users WHERE username = '" ^
    (real_escape conn new_username) ^ "'"
  in
  let query_result = exec conn sql_stmt in
  disconnect conn
  |> fun () -> if (size query_result) = Int64.zero then Lwt.return false else Lwt.return true

(* Check if the email_address already exists in the database *)
let email_exists new_email =
  let conn = connect user_db in
  let sql_stmt =
    "SELECT email FROM muz.users WHERE email = '" ^ (real_escape conn new_email) ^ "'"
  in
  let query_result = exec conn sql_stmt in
  disconnect conn
  |> fun () -> if (size query_result) = Int64.zero then Lwt.return false else Lwt.return true

(* Check that a password meets complexity requirements *)
let pwd_req_check pwd =

   (* At least 8 characters *)
   let length_check = if String.length pwd >= 8 then true else false in
   let length_msg =
     if length_check
     then ""
     else "The password must contain at least 8 characters."
   in
   (* At least 1 uppercase letter *)
   let uppercase_check =
     try (Str.search_forward (Str.regexp "[A-Z]") pwd 0) >= 0 with
     | Not_found -> false
   in
   let uppercase_msg =
     if uppercase_check
     then ""
     else "The password must contain at lease 1 uppercase character."
   in
   (* At least 3 numbers *)
   let number_check =
     Str.bounded_full_split (Str.regexp "[0-9]") pwd 0
     |> List.filter (fun x -> match x with Str.Delim _ -> true | _ -> false)
     |> List.length >= 3
   in
   let number_msg =
     if number_check
     then ""
     else "The password must contain at least 3 numbers."
   in
   (* Less than 100 characters *)
   let max_len_check = if String.length pwd <= 100 then true else false in
   let max_len_msg =
     if max_len_check
     then ""
     else "The password length must not contain more than 100 characters."
   in
   (* No Spaces Allowed *)
   let spaces_check =
     try if (Str.search_forward (Str.regexp " ") pwd 0) >= 0 then false else true with
     | Not_found -> true
   in
   let spaces_msg =
     if spaces_check
     then ""
     else "The password cannot contain any spaces."
   in

   match length_check, uppercase_check, number_check, max_len_check, spaces_check with
   | true, true, true, true, true -> true, ""
   | _, _, _, _, _ ->
       false, ("Error: " ^ length_msg ^ uppercase_msg ^ number_msg ^ max_len_msg ^ spaces_msg)

(* Connect, write a new user to the a database and disconnect *)
let write_new_user (u : user) pwd =
  match u.username with
  | Some un ->
      (
        let conn = connect user_db in
        let esc s = Mysql.real_escape conn s in
        username_exists un
        >>= fun b ->
          (
            if b then (disconnect conn |> fun () -> Lwt.return @@ "Username already exists")
            else
              let g = string_of_option in
              (* Salt and hash the password before storing *)
              let pwd' =
                Bcrypt.hash ~count:Config.pwd_iter_count (esc pwd) |> Bcrypt.string_of_hash
              in
              let sql_stmt =
                "INSERT INTO muz.users (username, email, password)" ^
                " VALUES('" ^ (esc @@ g u.username) ^ "', '" ^ (esc @@ g u.email) ^ "', '" ^
                (esc pwd') ^ "')"
              in
              let _ = exec conn sql_stmt in
              let user_id_sql_stmt =
                "SELECT user_id FROM muz.users WHERE username = '" ^ (esc @@ g u.username) ^ "'"
              in
              let user_id_query_result = exec conn user_id_sql_stmt in
              let user_id =
                try user_id_query_result |> sll_of_res |> List.hd |> List.hd
                (* TODO: log an error here *)
                with Failure hd -> raise (Failure "user_id not found")
              in
              let location_sql_stmt =
                "INSERT INTO muz.user_location (user_id)" ^
                "VALUES('" ^ user_id ^ "')"
              in
              let _ = exec conn location_sql_stmt in
              disconnect conn |> fun () -> Lwt.return "Username successfully created"
          )
      )
  | None -> Lwt.return "No username found"

(* Verify a username and password pair *)
let verify_login username pwd =
  let conn = connect user_db in
  let esc s = Mysql.real_escape conn s in
  let sql_stmt =
    "SELECT username, password FROM muz.users WHERE username = '" ^ (esc username) ^"'"
  in
  let query_result = exec conn sql_stmt in
  let name_pass =
    try query_result |> sll_of_res |> List.hd
    with Failure hd -> ["username fail"; "password fail"]
  in
  let verified =
    try
      List.nth name_pass 0 = username &&
      Bcrypt.verify (esc pwd) (Bcrypt.hash_of_string @@ esc @@ List.nth name_pass 1)
    with Bcrypt.Bcrypt_error -> false
  in
  disconnect conn;
  Lwt.return verified

(* Clean a users string of hashtags into a csv to be stored in the db *)
let csv_of_hashtags hashtags =
  Str.split (Str.regexp "[#]") hashtags
  |> List.map (fun s -> Str.global_replace (Str.regexp "[ ]") "" s)
  |> List.filter (fun s -> s <> "")
  |> String.concat ","

(* Write a new story to the database *)
let write_new_story (u : user) ~title ~body ~pic_link ~hashtags =
  let now = string_of_int @@ int_of_float @@ Unix.time () in
  let conn = connect user_db in
  let esc s = Mysql.real_escape conn s in
  let sql_stmt =
    "INSERT INTO muz.stories (username, title, body, pic_link, date_time, hashtags)" ^
    " VALUES ('" ^
    (esc @@ string_of_option u.username) ^ "', '" ^ (esc title) ^ "', '" ^ (esc body) ^ "', '" ^
    (esc @@ string_of_option pic_link) ^ "', '" ^ (esc now) ^ "', '" ^
    (esc @@ csv_of_hashtags hashtags) ^ "')"
  in
  let _ = exec conn sql_stmt in
  Lwt.return @@ disconnect conn

(* Get the most recent story from the database *)
let get_newest_story () =
  let conn = connect user_db in
  let sql_stmt_1 = "SELECT MAX(story_id) FROM muz.stories" in
  let query_result_1 = exec conn sql_stmt_1 in
  let max_id = query_result_1 |> sll_of_res |> List.hd |> List.hd in
  let sql_stmt_2 = "SELECT * FROM muz.stories WHERE story_id = " ^ max_id in
  let res = exec conn sql_stmt_2 |> sll_of_res |> List.hd in
  disconnect conn;
  Lwt.return @@ story_of_result res

(* Get a single story from the database, return Some story, or None *)
let get_story story_id =
  let conn = connect user_db in
  let sql_stmt = "SELECT * FROM muz.stories WHERE story_id = " ^ story_id in
  let res =
    try
      exec conn sql_stmt |> sll_of_res |> List.hd |> (fun s -> Some (story_of_result s))
    with
      Failure _ -> None
  in
  disconnect conn;
  Lwt.return @@ res

(* Get a stories for a single user *)
let get_all_stories username =
  let conn = connect user_db in
  let esc s = Mysql.real_escape conn s in
  let sql_stmt = "SELECT * FROM muz.stories WHERE username = " ^ "'" ^ (esc @@ username) ^ "'" in
  let query_result = exec conn sql_stmt in
  disconnect conn;
  try query_result |> sll_of_res |> (List.map story_of_result)
  with Failure hd -> []

(* Get the most recent stories *)
let get_recent_stories ~n () =
  let conn = connect user_db in
  let sql_stmt = "SELECT * FROM muz.stories ORDER BY date_time DESC LIMIT " ^ (string_of_int n) in
  let query_result = exec conn sql_stmt in
  disconnect conn;
  Lwt.return (
    try query_result |> sll_of_res |> (List.map story_of_result)
    with Failure hd -> []
  )

(* Given a csv of hashtags, sort the list by the highest count *)
let rec sort_tags ?(sorted_tags = []) (tag_list : string list) =
  match tag_list with
  | [] -> List.rev @@ List.sort (fun x y -> compare (snd x) (snd y)) sorted_tags
  | _ ->
    let new_tag = List.hd tag_list in
    let new_tag_count = List.filter (fun x -> x = new_tag) tag_list |> List.length in
    let remaining_list = List.filter (fun x -> x <> new_tag) tag_list in
    sort_tags ~sorted_tags:((new_tag, new_tag_count) :: sorted_tags) remaining_list

(* Get n elements from a list *)
let rec get_n ?(l_out = []) ~n l =
  match l with
  | [] -> List.rev l_out
  | hd :: tl ->
    if List.length l_out < n
    then get_n ~l_out:(hd :: l_out) ~n tl
    else List.rev l_out

(* Get hashtags for all stories in the last 24 hours *)
let get_recent_hashtags ~n () =
  let conn = connect user_db in
  let now = int_of_float @@ Unix.time () in
  let one_day_ago = string_of_int @@ now - 86400 in
  let sql_stmt =
    "SELECT hashtags FROM muz.stories WHERE date_time > " ^ one_day_ago ^
    " ORDER BY date_time DESC"
  in
  let query_result = exec conn sql_stmt in
  disconnect conn;
  let csv_tags =
    (
      try
        query_result |>
        sll_of_res |>
        List.map (fun sl -> List.hd sl |> (fun s -> s ^ ",")) |>
        List.fold_left (^) ""
      with
        Failure hd -> ""
    )
  in
  let sl_tags = Str.split (Str.regexp "[,]") csv_tags in
  Lwt.return (sort_tags sl_tags |> List.map (fun (s, i) -> s) |> get_n ~n)

(* Get all stories with a specific hashtag - Limit to 100 *)
let get_stories_by_hashtag hashtag =
  let conn = connect user_db in
  let sql_stmt = "SELECT * FROM muz.stories WHERE hashtags LIKE '%" ^ hashtag ^ "%'" in
  let query_result = exec conn sql_stmt in
  disconnect conn;
  try query_result |> sll_of_res |> (List.map story_of_result)
  with Failure hd -> []

(* Get all stories with a specific hood - Limit to 100 *)
let get_stories_by_hood hood =
  let conn = connect user_db in
  let sql_stmt =
    "SELECT story_id, stories.username, title, body, pic_link, date_time, hashtags, " ^
    "thumbs_up, thumbs_down " ^
    "FROM stories " ^
    "INNER JOIN users ON stories.username = users.username " ^
    "WHERE stories.username IN (" ^
    "SELECT username FROM users " ^
    "INNER JOIN user_location ON users.user_id = user_location.user_id " ^
    "WHERE hood = '" ^ hood ^ "')"
  in
  let query_result = exec conn sql_stmt in
  disconnect conn;
  try query_result |> sll_of_res |> (List.map story_of_result)
  with Failure hd -> []

(* Get the location information for a user *)
let get_user_location_info username =
  let conn = connect user_db in
  let sql_stmt =
    "SELECT country, state, city, hood, school FROM users " ^
    "INNER JOIN user_location ON users.user_id = user_location.user_id " ^
    "WHERE username = '" ^ username ^ "'"
  in
  let res =
    try exec conn sql_stmt |> sll_of_res |> List.hd |> location_of_result
    with _ -> {country = None; state = None; city = None; hood = None; school = None}
  in
  disconnect conn;
  res

(* Get the list of users who have rate a story with thumbs up / down *)
let get_thumbs ~up_down id =
  let ud =
    match up_down with
    | `Up -> "thumbs_up"
    | `Down -> "thumbs_down"
  in
  let conn = connect user_db in
  let sql_stmt = "SELECT " ^ ud ^ " FROM muz.stories WHERE story_id = " ^ id in
  let query_result = exec conn sql_stmt in
  disconnect conn;
  try query_result |> sll_of_res |> List.hd |> List.hd |> sl_of_csv with
  | Failure hd -> [] (* TODO: Log the error *)
  | _ -> []

(* Remove a users thumbs up / down action *)
let remove_thumbs_action ~up_down ~id username =
  let existing_thumbs = get_thumbs ~up_down id in
  if not (List.mem username existing_thumbs)
  then ()
  else
    let ud =
      match up_down with
      | `Up -> "thumbs_up"
      | `Down -> "thumbs_down"
    in
    let existing_thumbs' = List.filter (fun s -> s <> username) existing_thumbs in
    let users = String.concat "," existing_thumbs' in
    let conn = connect user_db in
    let sql_stmt = "UPDATE muz.stories SET " ^ ud ^ " = '" ^ users ^ "' WHERE story_id = " ^ id in
    let _ = exec conn sql_stmt in
    disconnect conn

(* Add a users thumbs up / down action, allowing the user to reverse a prior decision *)
let write_thumbs_action ~up_down ~id username =
  let t_ups = get_thumbs ~up_down:`Up id in
  let t_downs = get_thumbs ~up_down:`Down id in
  (* Undo the existing thumbs up / down if the user changes their mind *)
  let () =
    match up_down with
    | `Up ->
      if List.mem username t_downs
      then remove_thumbs_action ~up_down:`Down ~id username
      else ()
    | `Down ->
      if List.mem username t_ups
      then remove_thumbs_action ~up_down:`Up ~id username
      else ()
  in
  (* Write the new thumbs up / down action chosen by the user *)
  let existing_thumbs =
    match up_down with
    | `Up -> t_ups
    | `Down -> t_downs
  in
  if List.mem username existing_thumbs
  then ()
  else
    let ud =
      match up_down with
      | `Up -> "thumbs_up"
      | `Down -> "thumbs_down"
    in
    let users = (String.concat "," existing_thumbs) ^ "," ^ username in
    let conn = connect user_db in
    let sql_stmt = "UPDATE muz.stories SET " ^ ud ^ " = '" ^ users ^ "' WHERE story_id = " ^ id in
    let _ = exec conn sql_stmt in
    disconnect conn
