(* Convert a Caml directive argument to a human-readable string that will be
 * put in a comment. *)
let string_of_directive_argument : Parsetree.directive_argument -> string = function
    | Parsetree.Pdir_none -> "none"
    | Parsetree.Pdir_string s -> "\"" ^ s ^ "\""
    | Parsetree.Pdir_int i -> string_of_int i
    | Parsetree.Pdir_ident x -> String.concat " " (Longident.flatten x)
    | Parsetree.Pdir_bool b -> string_of_bool b

let rec coq_expression_of_caml_expression (pattern, {Parsetree.pexp_desc=expression; Parsetree.pexp_loc=loc}) : Coqtree.structure_item =
    let rec aux = function
    | Parsetree.Pexp_ident _ -> failwith "ident not implemented."
    | Parsetree.Pexp_constant _ -> failwith "constant not implemented."
    | Parsetree.Pexp_let _ -> failwith "let not implemented."
    | Parsetree.Pexp_function (label, expr, l) -> Coqtree.Definition ("foo", List.map coq_expression_of_caml_expression l)
    | Parsetree.Pexp_apply _ -> failwith "apply not implemented."
    | Parsetree.Pexp_match _ -> failwith "match not implemented."
    | Parsetree.Pexp_try _ -> failwith "try not implemented."
    | Parsetree.Pexp_tuple _ -> failwith "tuple not implemented."
    | Parsetree.Pexp_construct _ -> failwith "construct not implemented."
    | Parsetree.Pexp_variant _ -> failwith "variant not implemented."
    | Parsetree.Pexp_record _ -> failwith "record not implemented."
    | Parsetree.Pexp_field _ -> failwith "field not implemented."
    | Parsetree.Pexp_setfield _ -> failwith "setfield not implemented."
    | Parsetree.Pexp_array _ -> failwith "array not implemented."
    | Parsetree.Pexp_ifthenelse _ -> failwith "ifthenelse not implemented."
    | Parsetree.Pexp_sequence _ -> failwith "sequence not implemented."
    | Parsetree.Pexp_while _ -> failwith "while not implemented."
    | Parsetree.Pexp_for _ -> failwith "for not implemented."
    | Parsetree.Pexp_constraint _ -> failwith "constraint not implemented."
    | Parsetree.Pexp_when _ -> failwith "when not implemented."
    | Parsetree.Pexp_send _ -> failwith "send not implemented."
    | Parsetree.Pexp_new _ -> failwith "new not implemented."
    | Parsetree.Pexp_setinstvar _ -> failwith "setinstvar not implemented."
    | Parsetree.Pexp_override _ -> failwith "override not implemented."
    | Parsetree.Pexp_letmodule _ -> failwith "letmodule not implemented."
    | Parsetree.Pexp_assert _ -> failwith "assert not implemented."
    | Parsetree.Pexp_assertfalse
    | Parsetree.Pexp_lazy _ -> failwith "assertfalse/lazy not implemented."
    | Parsetree.Pexp_poly _ -> failwith "poly not implemented."
    | Parsetree.Pexp_object _ -> failwith "object not implemented."
    | Parsetree.Pexp_newtype _ -> failwith "newtype not implemented."
    | Parsetree.Pexp_pack _ -> failwith "pack not implemented."
    | Parsetree.Pexp_open _ -> failwith "open not implemented."
    in aux expression

let rec coq_type_of_caml_type_ident : Longident.t -> Coqtree.type_ = function
    | Longident.Lident s -> Coqtree.SimpleType s
    | Longident.Ldot (t, s1) -> (match (coq_type_of_caml_type_ident t) with
        | Coqtree.SimpleType s2 -> Coqtree.SimpleType (String.concat "." [s2; s1])
        | _ -> failwith "dot not implemented for other types than SimpleType."
    )
    | Longident.Lapply (t1, t2) -> Coqtree.Abstraction (
        coq_type_of_caml_type_ident t1,
        coq_type_of_caml_type_ident t2
    )


and coq_type_of_caml_type { Parsetree.ptyp_desc=desc; _ } : Coqtree.type_ =
    match desc with
    | Parsetree.Ptyp_any -> failwith "any not implemented."
    | Parsetree.Ptyp_var s -> Coqtree.SimpleType s
    | Parsetree.Ptyp_arrow _ -> failwith "arrow not implemented."
    | Parsetree.Ptyp_tuple [] -> Coqtree.NullType
    | Parsetree.Ptyp_tuple [hd] -> coq_type_of_caml_type hd
    | Parsetree.Ptyp_tuple l -> (match (List.rev l) with
            (* We make it in reverse order because we want (foo * bar * baz) to become
             * foo -> bar -> bar, and neither (foo -> bar) -> baz or baz -> bar -> foo *)
            | (hd :: tl) -> List.fold_left (fun x y -> Coqtree.Abstraction (coq_type_of_caml_type y, x)) (coq_type_of_caml_type hd) tl
            | _ -> failwith "logic error"
    )
    | Parsetree.Ptyp_constr (loc, _) -> coq_type_of_caml_type_ident loc.Asttypes.txt
    | Parsetree.Ptyp_object _ -> failwith "object not implemented."
    | Parsetree.Ptyp_class _ -> failwith "class not implemented."
    | Parsetree.Ptyp_alias _ -> failwith "alias not implemented."
    | Parsetree.Ptyp_variant _ -> failwith "variant not implemented."
    | Parsetree.Ptyp_poly _ -> failwith "poly not implemented."
    | Parsetree.Ptyp_package _ -> failwith "package not implemented."

let inductive_constructor_of_type_variant variant_name (loc, types, foo, bar) : Coqtree.inductive_constructor =
    let variant_type = Coqtree.SimpleType variant_name in
    Coqtree.InductiveConstructor (loc.Asttypes.txt, Coqtree.Types ((List.map coq_type_of_caml_type types) @ [variant_type]))

let coq_structure_item_of_type (loc, decl) : Coqtree.structure_item =
    if (List.length decl.Parsetree.ptype_params) <> 0 then
        failwith "parametered types not supported."
    else (
        match decl.Parsetree.ptype_kind with
        | Parsetree.Ptype_abstract -> failwith "abstract not implemented."
        | Parsetree.Ptype_variant x -> Coqtree.Inductive (
            loc.Asttypes.txt,
            List.map (inductive_constructor_of_type_variant loc.Asttypes.txt) x
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
