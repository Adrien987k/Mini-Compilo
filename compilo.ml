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
  let rec compile_expr expr out op alone =
    match expr with
    | Var s ->
        let var = List.find (fun var -> 
                              let name, offset = var in
                              (String.compare name s) = 0) env
        in
        let _, offset = var in
        let out = 
        ("\tmovq   " ^ (string_of_int offset) ^ "(%rbp), %rax\n")
        :: out in
        if not alone then 
          (if op then "\tpushq   %rax\n" else "\tpopq   %rbx\n")
          :: out
        else out
    | Or (e1, e2) -> let out = (compile_expr e1 [] true false) @ out in
                     let out = (compile_expr e2 [] false false) @ out in
                     let out = "\torq   %rbx, %rax\n" :: out in
                     if not alone then  
                       (if op then "\tpushq   %rax\n" else "\tpopq   %rbx\n")
                       :: out
                     else
                       out
    | And (e1, e2) -> let out = (compile_expr e1 [] true false) @ out in
                      let out = (compile_expr e2 [] false false) @ out in
                      let out = "\tandq   %rbx, %rax\n" :: out in
                      if not alone then 
                        (if op then "\tpushq   %rax\n" else "\tpopq   %rbx\n")
                        :: out
                      else
                        out
    | Not e -> if op then (* Not is not working ! *)
                    let out = (compile_expr e [] false true) @ out in 
                    ("\tnotq   %rbx\n" ^
                    "\tpushq   %rax\n")
                    :: out 
               else let out = (compile_expr e [] false true) @ out in
                    ("\tnotq   %rbx\n" ^
                    "\tpopq   %rbx\n")
                    :: out
  in
  let output = compile_expr expr output true true in
  let output = List.rev output in
  let output = String.concat "" output in
  Printf.printf "%s" output

(* TODO réecrire la partie 1 avec offset + affichage *)

let _ = compile_aux (And(Var ("x1"),Var ("x2"))) (["x1", (-24); "x2", (-16); "x3", (-8)])
