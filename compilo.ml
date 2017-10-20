type boolexpr = Var of string
              | Or of boolexpr * boolexpr
              | And of boolexpr * boolexpr
              | Not of boolexpr

let compile_aux expr env =
  let nb_var = List.length env in
  let offset_argv offset = 
    let bytes = (nb_var + 1) * 8 in
    bytes - (- (offset))
  in
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
  let output = 
    List.fold_left (fun acc var -> let name, _  = var in
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
  let output = 
    List.fold_left (fun acc var -> let name, offset = var in
      ((
        "\tmovq   24(%rbp), %rax\n" ^
        "\tmovq   " ^ (string_of_int (offset_argv offset)) ^ "(%rax), %rax\n" ^
        "\tmovq   %rax, %rdi\n" ^
        "\tmovq   $0, %rax\n" ^
        "\tcall   atoi\n" ^
        "\tmovq   %rax, " ^ (string_of_int offset) ^ "(%rbp)\n"
      )
      :: acc)) output env in
  let output = ("\tmovq   $say, %rdi\n" ^
               "\tmovq   $0, %rax\n" ^
               "\tcall   printf\n")
               :: output in
  let output = 
    List.fold_left (fun acc var -> let name, offset = var in
      ((
        "\tmovq   " ^ (string_of_int offset) ^ "(rbp), %rax\n" ^
        "\tmovq   %rax, 16(%rsp)\n" ^
        "\tmovq   %rax, %rdx\n" ^
        "\tmovq   $" ^ name ^ ", %rsi\n" ^
        "\tmovq   $var, %rdi\n" ^
        "\tmovq   $0, %rax\n" ^
        "\tcall   printf\n"
      )
      :: acc)) output env in
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
    | Not e -> let out = (compile_expr e [] false true) @ out in 
               let out = "\tnotq   %rax\n" :: out in
               if not alone then
                 (if op then "\tpushq   %rax\n" else "\tpopq   %rbx\n")
                 :: out
               else
                 out
  in
  let output = compile_expr expr output true true in
  let output = ("\tmovq   %rax, %rsi\n" ^
                "\tmovq   $res, %rdi\n" ^
                "\tmovq   $0, %rax\n" ^
                "\tcall   printf\n" ^
                "\tmovq   8(%rsp), %rax\n" ^
                "\tcall   exit"
               ) :: output in
  let output = List.rev output in
  let output = String.concat "" output in
  Printf.printf "%s\n" output

(* 
let _ = compile_aux (Not(And(Var ("x1"), Not(Var ("x2")))))
        (["x1", (-32); 
          "x2", (-24); 
          "x3", (-16);
          "x4", (-8)])
*)

let compile expr = 
  let rec collect_vars expr vars = 
    match expr with
    | Var s -> s :: vars
    | Or (e1, e2)
    | And (e1, e2) -> (collect_vars e1 []) @ (collect_vars e2 vars)
    | Not e -> collect_vars e vars
  in
  let vars = collect_vars expr [] in
  let vars = List.mapi (fun i var -> var, (i + 1) * (-8)) vars in
  compile_aux expr vars

let _ = compile (Not(And(Var ("x1"), Not(Var ("x2")))))