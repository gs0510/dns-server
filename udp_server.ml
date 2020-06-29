(* 1. Create a socket
   2. Bind to a socket 
   3. Listen on the socket
   4. Print all the requests 
*)

(* HEADER: id -> used for reply, QR -> query or response, opcode  *)
let dns_query data = Bytes.to_string (Bytes.sub data 0 2)

let create_socket port =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0
  and addr = Unix.inet_addr_of_string "127.0.0.1" in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (addr, port));
  sock

let server_process sock =
  let message = Bytes.create 10000 in
  while true do
    match Unix.recvfrom sock message 0 10000 [] with
    | len, Unix.ADDR_INET (addr, port) ->
        Printf.printf "Client %s said %s %s\n%!"
          (Unix.gethostbyaddr addr).Unix.h_name
          (String.sub (Bytes.to_string message) 0 len)
          (dns_query message);
        ignore (Unix.sendto sock message 0 len [] (Unix.ADDR_INET (addr, port)))
    | _ -> assert false
  done

let main () =
  let port = int_of_string Sys.argv.(1) in
  let sock = create_socket port in
  server_process sock

;;
main ()
