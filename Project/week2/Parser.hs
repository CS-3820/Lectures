module Parser where

import Language

import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

spaceC :: Parser ()
spaceC = L.space
  space1                        
  (L.skipLineComment "%")
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceC

psFile :: Parser PSExpr
psFile = spaceC >> PSProcedure <$> (many psObject) <* eof

psObject :: Parser PSExpr
psObject =
  lexeme (char '/' >> PSLiteralName <$> some letterChar) <|>
  lexeme (PSExecutableName <$> (some letterChar)) <|>
  try (lexeme (PSReal <$> L.signed spaceC L.float)) <|>
  lexeme (PSInt <$> L.signed spaceC L.decimal) <|>
  lexeme (PSArray <$> between (single '[' >> spaceC) (single ']') (many psObject)) <|>
  lexeme (PSProcedure <$> between (single '{' >> spaceC) (single '}') (many psObject))

parseFromFile :: String -> IO (Either (ParseErrorBundle String Void) PSExpr)
parseFromFile file = runParser psFile file <$> readFile file


