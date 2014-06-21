type structure = structure_item list

and structure_item =
    | Definition
    | Fixpoint
    | Comment of string
    | SubStructure of structure

type toplevel_phrase =
    | Structure of structure
