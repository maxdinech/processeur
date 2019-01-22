{
  open Lexing
  open Parser
  exception LexingError of string
}

let bit = ['0'-'1']
let blanc = " " | "\t"
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9'] | '_'
let comment = ';' (alphanum | blanc)* '\n'
let ident = alphanum*

rule token = parse
  | eof      { EOF }
  | blanc    { token lexbuf }
  | '\n'     { Lexing.new_line lexbuf; token lexbuf }
  | comment  { Lexing.new_line lexbuf; token lexbuf }
  | bit+     { CBIN (int_of_string (Lexing.lexeme lexbuf)) }
  | '/'      { SLASH }
  | "lw"     { LW }
  | "ci"     { CI }
  | "di"     { DI }
  | "te"     { TE }
  | "incr"   { INCR }
  | "j"      { J }
  | "je"     { JE }
  | "jne"    { JNE }
  | ident    { IDENT (Lexing.lexeme lexbuf) }
  | _  { token lexbuf }
