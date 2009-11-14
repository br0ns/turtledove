signature Pi_TOKENS =
sig
type ('a,'b) token
type svalue
val RPAR:  'a * 'a -> (svalue,'a) token
val OUTPUT:  'a * 'a -> (svalue,'a) token
val NEW:  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val INPUT:  'a * 'a -> (svalue,'a) token
val ILLCH:  'a * 'a -> (svalue,'a) token
val IDE: (string) *  'a * 'a -> (svalue,'a) token
val EQUALS:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val DVBAR:  'a * 'a -> (svalue,'a) token
val CARET:  'a * 'a -> (svalue,'a) token
end
signature Pi_LRVALS=
sig
structure Tokens : Pi_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
