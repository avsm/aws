type headers = (string * string) list

type request_body =
[ 
   | `InChannel of int * Lwt_io.input_channel
   | `None
   | `String of string 
]

exception Http_error of (int * headers * string)

open Lwt
open Cohttp
open Cohttp_lwt_unix

let body_of_in_channel ic =
  let st = Lwt_stream.from (
    fun () ->
      try_lwt
         match_lwt Lwt_io.read ic with
         |"" -> return None
         |x -> return (Some x)
      with exn -> return None
    ) in
  Cohttp_lwt_body.body_of_stream st

let call ?(headers=[]) ?body meth uri =
  let headers = Header.of_list headers in
  let body =
    match body with 
    |None -> None
    |Some (`InChannel (_,ic)) -> body_of_in_channel ic
    |Some `None -> None
    |Some (`String x) -> Cohttp_lwt_body.body_of_string x
  in
  Client.call ~headers ?body ~chunked:false meth (Uri.of_string uri)
  >>= function
  | None -> 
      fail (Http_error (0, [], ""))
  | Some (res, body) ->
      let status = Response.status res in
      if status <> `OK then
        fail (Http_error ((Code.code_of_status status), [], ""))
      else begin
        let headers = Header.to_list (Response.headers res) in
        return (headers, body)
      end

let call_to_string ?headers ?body meth uri =
  call ?headers ?body meth uri
  >>= fun (headers, body) ->
  Cohttp_lwt_body.string_of_body body 
  >>= fun body ->
  return (headers, body)

let call_to_oc ?headers ?body meth uri oc =
  call ?headers ?body meth uri
  >>= fun (headers, body) ->
  let st = Cohttp_lwt_body.stream_of_body body in
  Lwt_stream.iter_s (Lwt_io.write oc) st
  >>= fun () ->
  return headers

let get ?headers uri = call_to_string ?headers `GET uri
let post ?headers ?body uri = call_to_string ?headers ?body `POST uri
let put ?headers ?body uri = call_to_string ?headers ?body `PUT uri
let delete ?headers uri = call_to_string ?headers `DELETE uri
let head ?headers uri = call_to_string ?headers `HEAD uri
let get_to_chan ?headers uri oc = call_to_oc ?headers `GET uri oc
