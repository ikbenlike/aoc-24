import Data.Char
import Control.Applicative ((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

stringToNum :: String -> Int
stringToNum s = f s $ (length s - 1)
  where f [] _ = 0
        f (x:xs) n = digitToInt x * 10 ^ n + f xs (n - 1)

data Instruction = Mul Int Int
  | Enable
  | Disable
  | Zero
  deriving (Show, Eq)

parseMul :: Parser Instruction
parseMul = do
  string "mul("
  a <- many1 digit
  char ','
  b <- many1 digit
  char ')'
  pure $ Mul (stringToNum a) (stringToNum b)

parseEnable :: Parser Instruction
parseEnable = do
  string "do()"
  pure Enable

parseDisable :: Parser Instruction
parseDisable = do
  string "don't()"
  pure Disable

parseInstruction :: Parser Instruction
parseInstruction = parseMul <|> try parseEnable <|> parseDisable

any0 :: Parser Instruction
any0 = do
  anyToken
  pure Zero

body :: Parser [Instruction]
body = do
  a <- many (try parseInstruction <|> any0)
  pure a

final fp = fmap (foldl f (0, True)) <$> parse body "" <$> readFile fp
  where f (x, True) (Mul a b) = (x + a * b, True)
        f (x, False) (Mul a b)  = (x, False)
        f (x, _) Enable = (x, True)
        f (x, _) Disable = (x, False)
        f (x, b) Zero = (x, b)
