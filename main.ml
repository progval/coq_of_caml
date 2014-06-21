let compile e =
  begin
    print_string "ok.";
  end

let camltree_of_channel channel = Parser.use_file Lexer.token (Lexing.from_channel channel)

let _ =
    try
        let caml_tree = camltree_of_channel stdin in
            List.map print_string (List.map String_of_coqtree.string_of_coqtree (Coqtree_of_camltree.coqtree_of_camltree caml_tree));
            flush stdout
    with Parsing.Parse_error -> (print_string "Syntax error.\n")
