module Language where

-- In PostScript a Dictionary is a map from Names to expressions.  For now
-- there is only one dictionary: the sysdict defined below.
-- Later we will see multiple dictionaries.
type Dictionary = [(String, PSExpr)]

-- This is the abstract syntax tree for a postscript program.
-- Note that we derive Eq and Ord.  Those typeclasses can aid the
-- implementation.
data PSExpr = PSInt Int | PSReal Double | PSBoolean Bool
            | PSArray [PSExpr] | PSProcedure [PSExpr]
            | PSLiteralName String | PSExecutableName String
            | PSOp PSBuiltin | PSDict Dictionary
  deriving (Eq, Ord, Show)

-- This is the list of all builtin operators.  You should implement those.
data PSBuiltin = PSdup
               | PSpop
               | PSexch
               | PSadd
               | PSsub
               | PSmul
               | PSdiv
               | PSmod
               | PSneg
               | PStruncate
               | PSsin
               | PScos
               | PSdef
               | PSdict
               | PSbegin
               | PSend
               | PSeq
               | PSne
               | PSgt
               | PSge
               | PSlt
               | PSle
               | PSifelse
               | PSrepeat
               --- Graphics ---
               | PSfill
               | PSstroke
               | PSgsave
               | PSgrestore
               | PStranslate
               | PSrotate
               | PSscale
               | PSnewpath
               | PSmoveto
               | PSlineto
               | PSclosepath
               | PSsetlinewidth
               | PSsetrgbcolor
  deriving (Eq, Ord, Show)

-- The sysdict is a dictionary that always exists.  It maps the names of the
-- builtin operators to their internal representation.  When you encounter a
-- name, you should look it up from this dictionary.
sysdict :: Dictionary
sysdict = [
    ("dup",              PSOp PSdup),
    ("pop",              PSOp PSpop),
    ("exch",             PSOp PSexch),
    ("add",              PSOp PSadd),
    ("sub",              PSOp PSsub),
    ("mul",              PSOp PSmul),
    ("div",              PSOp PSdiv),
    ("mod",              PSOp PSmod),
    ("neg",              PSOp PSneg),
    ("truncate",         PSOp PStruncate),
    ("sin",              PSOp PSsin),
    ("cos",              PSOp PScos),
    ("def",              PSOp PSdef),
    ("dict",             PSOp PSdict),
    ("begin",            PSOp PSbegin),
    ("end",              PSOp PSend),
    ("eq",               PSOp PSeq),
    ("ne",               PSOp PSne),
    ("gt",               PSOp PSgt),
    ("ge",               PSOp PSge),
    ("lt",               PSOp PSlt),
    ("le",               PSOp PSle),
    ("ifelse",           PSOp PSifelse),
    ("repeat",           PSOp PSrepeat),
    ("fill",             PSOp PSfill),
    ("stroke",           PSOp PSstroke),
    ("gsave",            PSOp PSgsave),
    ("grestore",         PSOp PSgrestore),
    ("translate",        PSOp PStranslate),
    ("rotate",           PSOp PSrotate),
    ("scale",            PSOp PSscale),
    ("newpath",          PSOp PSnewpath),
    ("moveto",           PSOp PSmoveto),
    ("lineto",           PSOp PSlineto),
    ("closepath",        PSOp PSclosepath),
    ("setlinewidth",     PSOp PSsetlinewidth),
    ("setrgbcolor",      PSOp PSsetrgbcolor)
  ]

-- This is the type of the result of your interpreter.
type Result s = Either (String, s) s

