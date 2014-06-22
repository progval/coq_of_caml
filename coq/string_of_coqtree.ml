let rec string_of_type : Coqtree.type_ -> string = function
    | Coqtree.NullType -> "(nulltype)"
    | Coqtree.SimpleType s -> s
    | Coqtree.Product (t1, t2) -> String.concat "" ["("; string_of_type t1; ", "; string_of_type t2; ")"]
    | Coqtree.Abstraction (Coqtree.SimpleType t1, t2) -> String.concat " -> " [t1; string_of_type t2]
    | Coqtree.Abstraction (t1, t2) -> String.concat "" ["("; string_of_type t1; ") -> "; string_of_type t2]

let string_of_inductive_constructor (Coqtree.InductiveConstructor (name, types)) : string =
    match types with
    | Coqtree.Types []
    | Coqtree.Types [_] -> "| " ^ name
    | Coqtree.Types l -> String.concat " " ["|"; name; ":"; String.concat " -> " (List.map string_of_type l)]

let rec string_of_structure_item_list : Coqtree.structure_item list -> string = function
    | [] -> "\n(* ** *)\n"
    | [it] -> (string_of_structure_item true it) ^ ".\n"
    | (hd :: tl) -> String.concat "\n" ((string_of_structure_item true hd) :: (List.map (string_of_structure_item false) tl)) ^ ".\n"

and string_of_structure_item (prefix : bool) : Coqtree.structure_item -> string = function
    | Coqtree.Definition -> failwith "definition not implemented."
    | Coqtree.Inductive (id, constructors) ->
            (String.concat "" [if prefix then "Inductive " else "with "; id; " : Type :=\n"; String.concat "\n" (List.map string_of_inductive_constructor constructors)])
    | Coqtree.Fixpoint -> failwith "fixpoint not implemented."
    | Coqtree.Comment s -> s
    | Coqtree.SubStructure l -> string_of_structure_item_list l

let string_of_coqtree = function
    Coqtree.Structure l -> string_of_structure_item_list l
