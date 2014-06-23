type identifier = string

type type_ =
    | NullType
    | SimpleType of string
    | Product of type_ * type_
    | Abstraction of type_ * type_ (* type1 -> type2 *)

type types =
    | Types of type_ list

type inductive_constructor = 
    | InductiveConstructor of string * types

type structure = structure_item list

and structure_item =
    | Definition of identifier * types
    | Inductive of identifier * (inductive_constructor list)
    | Fixpoint
    | Comment of string
    | SubStructure of structure
    | StructureType of type_

type toplevel_phrase =
    | Structure of structure
