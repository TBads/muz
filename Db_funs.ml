(* MySql database functions *)

open Mysql

let (>>=) = Lwt.bind

(* User type *)
type user = {
  username : string option;
  email    : string option;
  verified : bool option
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
              disconnect conn |> fun () -> Lwt.return "Username successfully created"
          )
      )
  | None -> Lwt.return "No username found"
