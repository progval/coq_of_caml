let rec string_of_type : Coqtree.type_ -> string = function
    | Coqtree.NullType -> "(nulltype)"
    | Coqtree.SimpleType s -> s
    | Coqtree.Product (t1, t2) -> String.concat "" ["("; string_of_type t1; ", "; string_of_type t2; ")"]
    | Coqtree.Abstraction (Coqtree.SimpleType t1, t2) -> String.concat " -> " [t1; string_of_type t2]
    | Coqtree.Abstraction (t1, t2) -> String.concat "" ["("; string_of_type t1; ") -> "; string_of_type t2]

let string_of_inductive_constructor (Coqtree.InductiveConstructor (name, types)) : string =
    match types with
    | Coqtree.Types [] -> "| " ^ name
    | Coqtree.Types l -> String.concat " " ["|"; name; ":"; String.concat " -> " (List.map string_of_type l)]

let rec string_of_structure_item_list l : string =
    String.concat "(* ** *)" (List.map string_of_structure_item l)

and string_of_structure_item = function
    | Coqtree.Definition -> failwith "definition not implemented."
    | Coqtree.Inductive (id, constructors) ->
            (String.concat "" ["Inductive "; id; " : Type :=\n"; String.concat "\n" (List.map string_of_inductive_constructor constructors)]) ^ ".\n"
    | Coqtree.Fixpoint -> failwith "fixpoint not implemented."
    | Coqtree.Comment s -> s
    | Coqtree.SubStructure l -> string_of_structure_item_list l

let string_of_coqtree = function
    Coqtree.Structure l -> string_of_structure_item_list l
