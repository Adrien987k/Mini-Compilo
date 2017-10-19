type boolexpr = Var of string
              | Or of boolexpr * boolexpr
              | And of boolexpr * boolexpr
              | Not of boolexpr

let env = [] (*  (string * int)  *)

let compile_aux expr env =
  let output = [] in
  let output = ".data
say:
         .string \"Variable values:\\n\"
.align 8
var:
.string \"  %s: %d\\n\"
.align 8
res:
.string \"Result: %d\\n\"
.align 8
               "
               :: output in
  let output = List.fold_left (fun acc var -> let name, _  = var in
                          ((" " ^ name ^ ":
                                  .asciz \"" ^ name ^ "\"
                                  .align 8\n")
                          :: acc)) output env in
  let output = ".text
.global main
main:
.type   main, @function\n" :: output in
  let output = List.rev output in
  let output = String.concat "" output in
  Printf.printf "%s" output

let _ = compile_aux (Var ("")) (["toto", 5; "tata", 7])
