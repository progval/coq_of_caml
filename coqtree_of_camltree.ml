(* Convert a Caml directive argument to a human-readable string that will be
 * put in a comment. *)
let string_of_directive_argument : Parsetree.directive_argument -> string = function
    | Parsetree.Pdir_none -> "none"
    | Parsetree.Pdir_string s -> "\"" ^ s ^ "\""
    | Parsetree.Pdir_int i -> string_of_int i
    | Parsetree.Pdir_ident x -> String.concat " " (Longident.flatten x)
    | Parsetree.Pdir_bool b -> string_of_bool b

let coq_expression_of_caml_expression (pattern, {Parsetree.pexp_desc=expression; Parsetree.pexp_loc=loc}) : Coqtree.structure_item =
    match (pattern, expression) with
    | (_, Parsetree.Pexp_ident _) -> Coqtree.Comment "ident"
    | (_, Parsetree.Pexp_constant _) -> failwith "constant not implemented."
    | (_, Parsetree.Pexp_let _) -> failwith "let not implemented."
    | (_, Parsetree.Pexp_function _) -> failwith "function not implemented."
    | (_, Parsetree.Pexp_apply _) -> failwith "apply not implemented."
    | (_, Parsetree.Pexp_match _) -> failwith "match not implemented."
    | (_, Parsetree.Pexp_try _) -> failwith "try not implemented."
    | (_, Parsetree.Pexp_tuple _) -> failwith "tuple not implemented."
    | (_, Parsetree.Pexp_construct _) -> failwith "construct not implemented."
    | (_, Parsetree.Pexp_variant _) -> failwith "variant not implemented."
    | (_, Parsetree.Pexp_record _) -> failwith "record not implemented."
    | (_, Parsetree.Pexp_field _) -> failwith "field not implemented."
    | (_, Parsetree.Pexp_setfield _) -> failwith "setfield not implemented."
    | (_, Parsetree.Pexp_array _) -> failwith "array not implemented."
    | (_, Parsetree.Pexp_ifthenelse _) -> failwith "ifthenelse not implemented."
    | (_, Parsetree.Pexp_sequence _) -> failwith "sequence not implemented."
    | (_, Parsetree.Pexp_while _) -> failwith "while not implemented."
    | (_, Parsetree.Pexp_for _) -> failwith "for not implemented."
    | (_, Parsetree.Pexp_constraint _) -> failwith "constraint not implemented."
    | (_, Parsetree.Pexp_when _) -> failwith "when not implemented."
    | (_, Parsetree.Pexp_send _) -> failwith "send not implemented."
    | (_, Parsetree.Pexp_new _) -> failwith "new not implemented."
    | (_, Parsetree.Pexp_setinstvar _) -> failwith "setinstvar not implemented."
    | (_, Parsetree.Pexp_override _) -> failwith "override not implemented."
    | (_, Parsetree.Pexp_letmodule _) -> failwith "letmodule not implemented."
    | (_, Parsetree.Pexp_assert _) -> failwith "assert not implemented."
    | (_, Parsetree.Pexp_assertfalse)
    | (_, Parsetree.Pexp_lazy _) -> failwith "assertfalse/lazy not implemented."
    | (_, Parsetree.Pexp_poly _) -> failwith "poly not implemented."
    | (_, Parsetree.Pexp_object _) -> failwith "object not implemented."
    | (_, Parsetree.Pexp_newtype _) -> failwith "newtype not implemented."
    | (_, Parsetree.Pexp_pack _) -> failwith "pack not implemented."
    | (_, Parsetree.Pexp_open _) -> failwith "open not implemented."

let coq_type_of_caml_type { Parsetree.ptyp_desc=desc; _ } : Coqtree.type_ =
    match desc with
    | Parsetree.Ptyp_any -> failwith "any not implemented."
    | Parsetree.Ptyp_var s -> Coqtree.SimpleType s
    | Parsetree.Ptyp_arrow _ -> failwith "arrow not implemented."
    | Parsetree.Ptyp_tuple _ -> failwith "tuple not implemented."
    | Parsetree.Ptyp_constr _ -> failwith "constr not implemented."
    | Parsetree.Ptyp_object _ -> failwith "object not implemented."
    | Parsetree.Ptyp_class _ -> failwith "class not implemented."
    | Parsetree.Ptyp_alias _ -> failwith "alias not implemented."
    | Parsetree.Ptyp_variant _ -> failwith "variant not implemented."
    | Parsetree.Ptyp_poly _ -> failwith "poly not implemented."
    | Parsetree.Ptyp_package _ -> failwith "package not implemented."

let inductive_constructor_of_type_variant (loc, types, foo, bar) : Coqtree.inductive_constructor =
    Coqtree.InductiveConstructor (loc.Asttypes.txt, Coqtree.Types (List.map coq_type_of_caml_type types))

let coq_structure_item_of_type (loc, decl) : Coqtree.structure_item =
    if (List.length decl.Parsetree.ptype_params) <> 0 then
        failwith "parametered types not supported."
    else (
        match decl.Parsetree.ptype_kind with
        | Parsetree.Ptype_abstract -> failwith "abstract not implemented."
        | Parsetree.Ptype_variant x -> Coqtree.Inductive (
            loc.Asttypes.txt,
            List.map inductive_constructor_of_type_variant x
        )
        | Parsetree.Ptype_record _ -> failwith "record not implemented."
    )

let coq_structure_of_caml_structure_item {Parsetree.pstr_desc=desc; Parsetree.pstr_loc=loc} : Coqtree.structure =
    match desc with
    | Parsetree.Pstr_eval _ -> failwith "eval not implemented."
    | Parsetree.Pstr_value (rec_flag, statements) ->
            List.map coq_expression_of_caml_expression statements
    | Parsetree.Pstr_primitive _ -> failwith "primitive not implemented."
    | Parsetree.Pstr_type l ->
            List.map coq_structure_item_of_type l
    | Parsetree.Pstr_exception _ -> failwith "exception not implemented."
    | Parsetree.Pstr_exn_rebind _ -> failwith "exn_rebind not implemented."
    | Parsetree.Pstr_module _ -> failwith "module not implemented."
    | Parsetree.Pstr_recmodule _ -> failwith "recmodule not implemented."
    | Parsetree.Pstr_modtype _ -> failwith "modtype not implemented."
    | Parsetree.Pstr_open _ -> failwith "open not implemented."
    | Parsetree.Pstr_class _ -> failwith "class not implemented."
    | Parsetree.Pstr_class_type _ -> failwith "class_type not implemented."
    | Parsetree.Pstr_include _ -> failwith "include not implemented."


let coqtree_of_camltree_aux : Parsetree.toplevel_phrase -> Coqtree.toplevel_phrase = function
    | Parsetree.Ptop_def x -> Coqtree.Structure (List.concat (List.map coq_structure_of_caml_structure_item x))
    | Parsetree.Ptop_dir (name, arg) ->
            Coqtree.Structure [Coqtree.Comment ("Directive " ^ ": " ^ (string_of_directive_argument arg))]

let coqtree_of_camltree =
    List.map coqtree_of_camltree_aux
