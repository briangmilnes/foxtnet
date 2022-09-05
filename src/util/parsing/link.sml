signature LINK_PARSER =
 sig
  structure Position: POSITION
  structure Parser: PARSER
  structure Input: INPUT
  structure Parsing_Utils: PARSING_UTILS
   sharing type Parser.P = Parsing_Utils.P
       and type Position.T = Parsing_Utils.T = Parser.Position.T
 end

functor Link_Parser (structure V: VENDOR) =
 struct
  structure Base = Base (structure V = V)
  structure Position = Position ()
  structure Parser_Base = ContParser (structure Position = Position)
  structure Parser = Parser (structure Base = Base
			     structure ParserBase = Parser_Base)
  structure Input = Input (structure Base = Base
			   structure V = V)
  structure Parsing_Utils = Parsing_Utils (structure V = V
					   structure Parser = Parser)
 end
