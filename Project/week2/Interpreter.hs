module Interpreter (interpret) where

import Language

import Graphics.Rendering.Cairo hiding (x)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List

-- The state is the current object list, the stack, and the directory
data State = State [PSExpr] [PSExpr] [Dictionary]

instance Show State where
  show (State c s d) = cstr ++ "\n" ++ sstr
    where
      cstr = case c of
        [] -> " Execution stack empty."
        _  -> " Execution Stack:\n  " ++ concat (intersperse "\n  " (map show c))
      sstr = case s of
        [] -> " Data stack empty."
        _  -> " Data Stack:\n  " ++ concat (intersperse "\n  " (map show s))
      dstr = case d of
        [] -> " Dictionary stack empty."
        _  -> " Dictionary Stack:\n  " ++ concat (intersperse "\n  " (map show (init s)))

emptyState :: State
emptyState = State [] [] []

interpret :: PSExpr -> Render (Result State)
interpret (PSProcedure cmds) = bigStepPs $ State cmds [] [[], sysdict]
interpret _ = return $ Left ("Unexpected toplevel symbol", emptyState)

lookupInDicts :: [Dictionary] -> String -> Maybe PSExpr
lookupInDicts [] _ = Nothing
lookupInDicts (x:xs) name = case lookup name x of
  Just e -> Just e
  Nothing -> lookupInDicts xs name

binNumOp :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> State -> Result State
binNumOp iop _   (State c ((PSInt i):(PSInt j):s)   d) = Right (State c ((PSInt  $ iop j i):s) d)
binNumOp _   dop (State c ((PSReal i):(PSReal j):s) d) = Right (State c ((PSReal $ dop j i):s) d)
binNumOp _   dop (State c ((PSInt i):(PSReal j):s)  d) = Right (State c ((PSReal $ dop j (fromIntegral i)):s) d)
binNumOp _   dop (State c ((PSReal i):(PSInt j):s)  d) = Right (State c ((PSReal $ dop (fromIntegral j) i):s) d)
binNumOp _ _ s = Left ("Illegal arguments to binary operator.", s)

-- Helper for unary arithmetic operators
unaryNumOp :: (Int -> Int) -> (Double -> Double) -> State -> Result State
unaryNumOp iop _   (State c ((PSInt i):s)  d) = Right (State c ((PSInt  $ iop i):s) d)
unaryNumOp _   dop (State c ((PSReal i):s) d) = Right (State c ((PSReal $ dop i):s) d)
unaryNumOp _ _ s = Left ("Illegal arguments to unary operator.", s)

-- Helper to handle the case where a recursion can fail
tryRecurse :: Result State -> Render (Result State)
tryRecurse r@(Left _) = return r
tryRecurse (Right s) = bigStepPs s

-- Main interpreter
bigStepPs :: State -> Render (Result State)
bigStepPs s@(State [] _ _) = return $ Right s
bigStepPs (State (i@(PSInt _):c)         s d) = bigStepPs (State c (i:s) d)
bigStepPs (State (r@(PSReal _):c)        s d) = bigStepPs (State c (r:s) d)
bigStepPs (State (b@(PSBoolean _):c)     s d) = bigStepPs (State c (b:s) d)
bigStepPs (State (a@(PSArray _):c)       s d) = bigStepPs (State c (a:s) d)
bigStepPs (State (p@(PSProcedure _):c)   s d) = bigStepPs (State c (p:s) d)

bigStepPs (State (b@(PSLiteralName _):c) s d) = bigStepPs (State c (b:s) d)

bigStepPs s@(State ((PSExecutableName n):cmds) stack dicts) =
  case lookupInDicts dicts n of
    Just (PSProcedure scmds) -> bigStepPs (State (scmds ++ cmds) stack dicts)
    Just e -> bigStepPs (State (e:cmds) stack dicts)
    Nothing ->  return $ Left ("name not found: " ++ n, s)

bigStepPs (State (PSOp PSdup:c)  (a:s) d)   = bigStepPs $ State c (a:a:s) d
bigStepPs (State (PSOp PSpop:c)  (_:s) d)   = bigStepPs $ State c      s  d
bigStepPs (State (PSOp PSexch:c) (a:b:s) d) = bigStepPs $ State c (b:a:s) d
bigStepPs (State (PSOp PSadd:c) s d) = tryRecurse $ binNumOp (+) (+) (State c s d)
bigStepPs (State (PSOp PSsub:c) s d) = tryRecurse $ binNumOp (-) (-) (State c s d)
bigStepPs (State (PSOp PSmul:c) s d) = tryRecurse $ binNumOp (*) (*) (State c s d)

bigStepPs (State (PSOp PSdiv:c) ((PSInt  j):(PSInt  i):s) d) = bigStepPs (State c (PSReal ((fromIntegral i) / (fromIntegral j)):s) d)
bigStepPs (State (PSOp PSdiv:c) ((PSReal j):(PSInt  i):s) d) = bigStepPs (State c (PSReal ((fromIntegral i) / j):s) d)
bigStepPs (State (PSOp PSdiv:c) ((PSInt  j):(PSReal i):s) d) = bigStepPs (State c (PSReal (i / (fromIntegral j)):s) d)
bigStepPs (State (PSOp PSdiv:c) ((PSReal j):(PSReal i):s) d) = bigStepPs (State c (PSReal (i / j):s) d)

bigStepPs (State (PSOp PSneg:c) s d) = tryRecurse $ unaryNumOp (0 -) (0.0 -) (State c s d)

bigStepPs (State (PSOp PSmod:c)  ((PSInt j):(PSInt i):s) d) = bigStepPs (State c (PSInt (i `mod` j):s) d)
bigStepPs (State (PSOp PStruncate:c) s d) = tryRecurse $ unaryNumOp id (fromIntegral . truncate) (State c s d)

bigStepPs (State (PSOp PSsin:c) ((PSInt i):s) d)  =
  bigStepPs (State c (PSReal (sin $ (fromIntegral i) / 180 * pi):s) d)
bigStepPs (State (PSOp PSsin:c) ((PSReal i):s) d) =
  bigStepPs (State c (PSReal (sin $ i / 180 * pi):s) d)

bigStepPs (State (PSOp PScos:c) ((PSInt i):s) d)  =
  bigStepPs (State c (PSReal (cos $ (fromIntegral i) / 180 * pi):s) d)
bigStepPs (State (PSOp PScos:c) ((PSReal i):s) d) =
  bigStepPs (State c (PSReal (cos $ i / 180 * pi):s) d)

bigStepPs (State (PSOp PSeq:c) (b:a:s) d) = bigStepPs (State c (PSBoolean (a == b):s) d)
bigStepPs (State (PSOp PSne:c) (b:a:s) d) = bigStepPs (State c (PSBoolean (a /= b):s) d)
bigStepPs (State (PSOp PSgt:c) (b:a:s) d) = bigStepPs (State c (PSBoolean (a > b):s)  d)
bigStepPs (State (PSOp PSge:c) (b:a:s) d) = bigStepPs (State c (PSBoolean (a >= b):s) d)
bigStepPs (State (PSOp PSlt:c) (b:a:s) d) = bigStepPs (State c (PSBoolean (a < b):s)  d)
bigStepPs (State (PSOp PSle:c) (b:a:s) d) = bigStepPs (State c (PSBoolean (a <= b):s) d)

bigStepPs (State (PSOp PSifelse:c) (PSProcedure no:PSProcedure yes:PSBoolean cond:s) d)
  | cond      = bigStepPs $ State (yes ++ c) s d
  | otherwise = bigStepPs $ State (no ++ c)  s d

-- Thise implementation of repeate is very naive: it simply copies to code n times
bigStepPs (State (PSOp PSrepeat:c) (PSProcedure code:PSInt n:s) d) = bigStepPs $ State c' s d
  where c' = (concat $ take n (repeat code)) ++ c

bigStepPs (State (PSOp PSnewpath:c) s d) = newPath >> (bigStepPs $ State c s d)

bigStepPs (State (PSOp PSmoveto:c) ((PSInt y):(PSInt x):s) d) =
  moveTo (fromIntegral x) (fromIntegral y) >> (bigStepPs $ State c s d)
bigStepPs (State (PSOp PSmoveto:c) ((PSReal y):(PSReal x):s) d) =
  moveTo x y                               >> (bigStepPs $ State c s d)

bigStepPs (State (PSOp PStranslate:c) ((PSReal y):(PSReal x):s) d) =
  translate x y >> (bigStepPs $ State c s d)
bigStepPs (State (PSOp PStranslate:c) ((PSInt y):(PSInt x):s) d) =
  translate (fromIntegral x) (fromIntegral y) >> (bigStepPs $ State c s d)
bigStepPs (State (PSOp PSscale:c) ((PSReal y):(PSReal x):s) d) =
  scale x y >> (bigStepPs $ State c s d)
bigStepPs (State (PSOp PSscale:c) ((PSInt y):(PSInt x):s) d) =
  scale (fromIntegral x) (fromIntegral y) >> (bigStepPs $ State c s d)
bigStepPs (State (PSOp PSrotate:c) ((PSInt r):s) d) =
  rotate ((fromIntegral r) / 180 * pi) >> (bigStepPs $ State c s d)
bigStepPs (State (PSOp PSrotate:c) ((PSReal r):s) d) =
  rotate (r / 180 * pi) >> (bigStepPs $ State c s d)

bigStepPs (State (PSOp PSclosepath:c) s d) = closePath >> (bigStepPs $ State c s d)

bigStepPs (State (PSOp PSlineto:c) ((PSInt y):(PSInt x):s) d) =
  lineTo (fromIntegral x) (fromIntegral y) >> (bigStepPs $ State c s d)
bigStepPs (State (PSOp PSlineto:c) ((PSReal y):(PSReal x):s) d) =
  lineTo x y                               >> (bigStepPs $ State c s d)

bigStepPs (State (PSOp PSsetlinewidth:c) ((PSInt w):s) d) =
  setLineWidth (fromIntegral w) >> (bigStepPs $ State c s d)
bigStepPs (State (PSOp PSsetlinewidth:c) ((PSReal w):s) d) =
  setLineWidth w                >> (bigStepPs $ State c s d)

bigStepPs (State (PSOp PSstroke:c) s d)   = stroke >> (bigStepPs $ State c s d)
bigStepPs (State (PSOp PSfill:c) s d)     = fill >> (bigStepPs $ State c s d)
bigStepPs (State (PSOp PSgsave:c) s d)    = save >> (bigStepPs $ State c s d)
bigStepPs (State (PSOp PSgrestore:c) s d) = restore >> (bigStepPs $ State c s d)


bigStepPs (State (PSOp PSsetrgbcolor:c) ((PSReal b):(PSReal g):(PSReal r):s) d) =
  setSourceRGB r g b >> (bigStepPs $ State c s d)

-- Code to handle dictionaries and definitions
bigStepPs (State (PSOp PSdict:c)         (_:s) d) = (bigStepPs $ State c (PSDict []:s) d)
bigStepPs (State (PSOp PSbegin:c) (PSDict x:s) d) = (bigStepPs $ State c s (x:d))
bigStepPs (State (PSOp PSend:c)   s (_:d)) = (bigStepPs $ State c s d)

bigStepPs (State (PSOp PSdef:c) (v:(PSLiteralName n):s) (d:ds)) = (bigStepPs $ State c s (((n, v):d):ds))

bigStepPs s = return $ Left ("No case for current state.", s)
