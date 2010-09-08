(* DOCUMENTATION

   SpaceV and SpaceH does what you expect except for one case:
     Normally the records of a row share the width evenly, but a SpaceH only
     uses as much space as it needs. This only holds when the SpaceH is a direct
     child of the row, but not for example if it lies within an indentation.

   Example: You want two columns separated by three spaces, and adjusted to the
            right. Each column should be at least 30 characters wide, and the
            leftmost should be indented at least 10 characters.
   This is what you want:
      Row [Fill,
           SpaceH 10,
           Col [SpaceH 30,
                Text {floath = Left,
                      floatv = Top,
                      format = Plain,
                      contents = <column1>}],
           SpaceH 3,
           Col [SpaceH 30,
                Text {floath = Left,
                      floatv = Top,
                      format = Plain,
                      contents = <column2>}]
          ]

   Note that you need a width of at least 10 + 30 + 3 + 30 = 73 in this example.

*)
signature Report =
sig
  structure FloatH :
            sig datatype t = Left | Center | Right end

  structure FloatV :
            sig datatype t = Top | Center | Bottom end

  structure Format :
            sig datatype t = Plain | Paragraph | Indent end

  structure BorderStyle :
            sig datatype t = StyleA | StyleB | StyleC end

  structure EnumStyle :
            sig datatype t = Number | Letter | Roman end

  structure Side :
            sig datatype t = Left | Right | Top | Bottom end

  structure Heading :
            sig datatype t = Both | Vertical | Horizontal end

  exception Width (* Output area is not wide enough *)

  datatype t = Text      of {floath      : FloatH.t,
                             floatv      : FloatV.t,
                             format      : Format.t,
                             contents    : string
                            }
             | Row       of t list
             | Col       of t list
             | Fill
             | SpaceH    of int
             | SpaceV    of int
             | Indent    of {indentation : int,
                             contents    : t
                            }
             | Itemize   of {bullet      : string option,
                             items       : t list
                            }
             | Enumerate of {style       : EnumStyle.t,
                             items       : t list
                            }
             | Border    of {borderstyle : BorderStyle.t,
                             sides       : Side.t list,
                             contents    : t
                            }
             | Table     of {borderstyle : BorderStyle.t,
                             heading     : Heading.t,
                             items       : t list list
                            }

  val text' : FloatH.t -> FloatV.t -> Format.t -> string -> t
  val text : string -> t
  val paragraph : string -> t
  val indent : t -> t
  val ++ : t * t -> t
  val @@ : t * t -> t
  val || : t * t -> t

  val itemize : t list -> t
  val itemize' : string -> t list -> t
  val itemizenl : t list -> t
  val itemizenl' : string -> t list -> t
  val enumerate : t list -> t
  val enumerate' : EnumStyle.t -> t list -> t
  val enumeratenl : t list -> t
  val enumeratenl' : EnumStyle.t -> t list -> t

  val row : t list -> t
  val column : t list -> t

  val nl : t

  val toString : t -> string
  val print : t -> unit
end
