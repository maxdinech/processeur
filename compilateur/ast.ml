open Lexing

type fichier  = Some  of prog  | None 
 and prog     = instr  list
 and prog_    = instr_ list
 and ident    = string
 and position = ident
 and registre = int
 and valeur   = int
 and instr_   = Lw of valeur * registre
              | Ci of valeur * registre
              | Di of registre * registre
              | Incr of registre * registre
              | Te of registre * registre
              | J of position
              | Je of position * registre * registre
              | Jne of position * registre * registre
              | Setpos of ident
 and instr    = instr_ * (Lexing.position * Lexing.position)

