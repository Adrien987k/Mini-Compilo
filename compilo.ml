type boolexpr = Var of string
              | Or of boolexpr * boolexpr
              | And of boolexpr * boolexpr
              | Not of boolexpr

let env = [] (*  (string * int)  *)

let compile_aux expr env =
  let nb_var = List.length env in
  let output = [] in
  let output = (".data\n" ^
               "\tsay:\n" ^
               "\t.string \"Variable values:\\n\"\n" ^
               "\t.align 8\n" ^
               "var:\n" ^
               "\t.string \"  %s: %d\\n\"\n" ^
               "\t.align 8\n" ^
               "res:\n" ^
               "\t.string \"Result: %d\\n\"\n" ^
               "\t.align 8\n")
               :: output in
  let output = List.fold_left (fun acc var -> let name, _  = var in
                          ((name ^ ":\n" ^
                            "\t.asciz \"" ^ name ^ "\"\n" ^
                            "\t.align 8\n")
                          :: acc)) output env in
  let output = (".text\n" ^
               "\t.global main\n" ^
               "main:\n" ^
               "\t.type   main, @function\n" ^
               "\tpushq   %rbp\n" ^
               "\tmovq   %rsp, %rbp\n") 
               :: output in
  let bytes = nb_var * 16 in
  let output = ("\tsubq   $" ^ (string_of_int bytes) ^ ", %rsp\n") 
               :: output in
  let rec collect_args n offset out =
    if n = 0 then out else
    let out = ("\tmovq   24(%rbp), %rax\n" ^
              "\tmovq   " ^ (string_of_int offset) ^ "(%rax), %rax\n" ^
              "\tmovq   %rax, %rdi\n" ^
              "\tmovq   $0, %rax\n" ^
              "\tcall   atoi\n" ^
              "\tmovq   %rax, " ^ (string_of_int (n * (-8))) ^ "(%rbp)\n")
              :: out in
    collect_args (n - 1) (offset + 8) out
  in
  let output = collect_args nb_var 8 output in
  let output = ("\tmovq   $say, %rdi\n" ^
               "\tmovq   $0, %rax\n" ^
               "\tcall   printf\n") 
               :: output in
  let out = (output, nb_var) in
  let out = 
    List.fold_left (fun acc var -> let name, _  = var in
                                   let out, n   = acc in
      (((
        "\tmovq   " ^ (string_of_int (n * (-8))) ^ "(rbp), %rax\n" ^
        "\tmovq   %rax, 16(%rsp)\n" ^
        "\tmovq   %rax, %rdx\n" ^
        "\tmovq   $" ^ name ^ ", %rsi\n" ^
        "\tmovq   $var, %rdi\n" ^
        "\tmovq   $0, %rax\n" ^
        "\tcall   printf\n"
      )
      :: out), (n - 1))) out env in
  let output, _ = out in
  let output = List.rev output in
  let output = String.concat "" output in
  Printf.printf "%s" output

let _ = compile_aux (Var ("")) (["toto", 5; "tata", 7; "var", 2])
