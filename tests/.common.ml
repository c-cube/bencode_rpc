let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ()
;;
#use "topfind";;

#require "unix";;
#require "lwt";;
#require "lwt.unix";;
#require "bencode";;

#directory "_build/src/";;
#load "bencode_rpc.cma";;

let ok () =
  print_endline "... OK";
  exit 0;;

let fail msg =
  print_endline ("... FAILURE: " ^ msg);
  exit 1;;

(* vim:syntax=ocaml
*)


