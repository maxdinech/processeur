%{
open Ast
%}
%token <int> CBIN 
%token <string> IDENT
%token EOF
%token SLASH "/"
%token LW CI DI TE INCR J JE JNE

%start fichier
%type <Ast.fichier> fichier

%%

fichier:
  | f = list(instr_) ; EOF { Some f }
  | EOF { None }

instr_:
  | i = instr  { i, ($startpos, $endpos) }

instr:
  | LW v = valeur ; r = reg  { Lw (v, r) }
  | CI v = valeur ; r = reg  { Ci (v, r) }
  | DI r1 = reg ; r2 = reg  { Di (r1, r2) }
  | TE r1 = reg ; r2 = reg  { Te (r1, r2) }
  | INCR r1 = reg ; r2 = reg  { Incr (r1, r2) }
  | J   p = pos  { J p }
  | JE  p = pos ; r1 = reg ; r2 = reg  { Je  (p, r1, r2) }
  | JNE p = pos ; r1 = reg ; r2 = reg  { Jne (p, r1, r2) }
  | "/" p = pos  { Setpos p }

reg:
  | i = CBIN  { i }

valeur:
  | i = CBIN  { i }

pos:
  i = IDENT  { i }
