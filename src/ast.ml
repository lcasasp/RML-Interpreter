(** The abstract syntax tree type. *)

(******************************************************************************
   These types (id, handle, uop, bop) are used by the parser and type-checker.
   You do not want to change them.
 ******************************************************************************)

type id = string
type handle = int

type bop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | Cat

and uop =
  | Neg
  | Not

(******************************************************************************
   [pat] is the type of the AST for patterns. You may implement
   this type however you wish. Look at the formal semantics and think about other
   AST types we have worked with for inspiration.
 ******************************************************************************)

type pat =
  | PWild 
  | PVar of id 
  | PUnit 
  | PBool of bool
  | PInt of int 
  | PString of string
  | PPair of pat * pat
  | PNil
  | PCons of pat * pat

(******************************************************************************
   [expr] is the type of the AST for expressions. You may implement
   this type however you wish.  Use the example interpreters seen in
   the textbook as inspiration.
 ******************************************************************************)

type expr =
  | EUnit of unit
  | EBool of bool
  | EInt of int
  | EString of string
  | EVar of id
  | EUop of uop * expr
  | EBop of bop * expr * expr
  | ELet of pat * expr * expr
  | EFun of pat * expr
  | EApp of expr * expr
  | EIf of expr * expr * expr
  | EPair of expr * expr
  | ENil
  | ECons of expr * expr
  | ESeq of expr * expr
  | EMatch of expr * (pat * expr) list
  | ERef of expr
  | EAssign of expr * expr
  | EDeref of expr

(******************************************************************************
   [defn] is the type of the AST for definitions. You may implement this type
   however you wish.  There are only two kinds of definition---the let
   definition and the let [rec] definition---so this type can be quite simple.
 ******************************************************************************)
and defn =
  | DLet of pat * expr

(******************************************************************************
   [prog] is the type of the AST for an RML program. You should 
   not need to change it.
 ******************************************************************************)

type prog = defn list
