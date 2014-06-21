let rec string_of_structure_item_list l =
    String.concat "(* ** *)" (List.map string_of_structure_item l)

and string_of_structure_item = function
    | Coqtree.Definition -> failwith "definition not implemented."
    | Coqtree.Fixpoint -> failwith "fixpoint not implemented."
    | Coqtree.Comment s -> s
    | Coqtree.SubStructure l -> string_of_structure_item_list l

let string_of_coqtree = function
    Coqtree.Structure l -> string_of_structure_item_list l
