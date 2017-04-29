import Control.Monad.State
import Control.Applicative
import Control.Monad
import Data.Char
import System.IO
import qualified Library.EmulateTM as E


type TapeSymbol = Char
type StateQ = String
type SingleDelta = (StateQ, TapeSymbol, StateQ, TapeSymbol, TapeSymbol)
type DeltaData = (TapeSymbol, StateQ, TapeSymbol, TapeSymbol)
type Delta = (StateQ, [DeltaData])

-- タプルから値を取得する関数
get1st :: DeltaData -> TapeSymbol
get1st (x,_,_,_) = x

get2nd :: DeltaData -> StateQ
get2nd (_,x,_,_) = x

get3rd :: DeltaData -> TapeSymbol
get3rd (_,_,x,_) = x

get4th :: DeltaData -> TapeSymbol
get4th (_,_,_,x) = x

-- 文字列操作用の関数
splitBySemicolon :: String -> [String]
splitBySemicolon xs = fst h : case snd h of
  [] -> []
  (_:xs') -> splitBySemicolon xs'
  where h = break (== ';') xs

removeLn :: String -> String
removeLn [] = []
removeLn (x:xs)
  | x == '\n'   = removeLn xs
  | otherwise   = x : removeLn xs

removeSpace :: String -> String
removeSpace [] = []
removeSpace (x:xs)
  | x == ' '    = removeSpace xs
  | otherwise   = x : removeSpace xs

getDelta' :: String -> String
getDelta' [] = []
getDelta' (x:xs)
  | d x       = ' ' : getDelta' xs
  | otherwise = x : getDelta' xs
    where d = (\x' -> x' == '(' || x' == ')'
                                || x' == '-'
                                || x' == '>'
                                || x' == ',')

getDelta :: [String] -> Maybe SingleDelta
getDelta xs = if length xs == 5 then f xs
                                else Nothing
  where f = \(s0:s1:s2:s3:s4:s) -> if length s1 == 1 && length s3 == 1
                                                     && length s4 == 1
                                     then Just (s0, head s1, s2, head s3, head s4)
                                     else Nothing

convertMult :: SingleDelta -> Maybe Delta
convertMult (s0, c0, s1, c1, c2) = Just (s0, [(c0, s1, c1, c2)])

(-:) :: Delta -> Maybe [Delta] -> Maybe [Delta]
(-:) d ds = do
  ds' <- ds
  return (d : ds')

(-:-) :: Delta -> Delta -> Delta
(-:-) x y = (fst x, snd x ++ (snd y))

concatDelta :: [Delta] -> Maybe [Delta]
concatDelta [] = return []
concatDelta [x] = return [x]
concatDelta (x:y:zs) = if fst x == fst y then concatDelta ((x -:- y):zs)
                                         else x -: concatDelta (y:zs)

makeFunction' :: DeltaData -> Maybe String
makeFunction' (c0, s0, c1, c2) = Just ('\'' : c0 : "' -> do\n      E.rp '" ++ [c1] ++"'\n      E.m" ++ [chr $ (ord 'a' - ord 'A') + (ord c2)] ++ "\n      q" ++ s0 ++ "\n    ")

makeFunction :: Delta -> Maybe String
makeFunction d = do
  let initStr = 'q' : (fst d) ++ " = do\n  s <- get\n  case head $ snd s of\n    "
      lastStr = "_ -> do\n      E.returnError\n"
      d' = snd d
  funcStr <- forM d' makeFunction'
  return $ initStr ++ (concat funcStr) ++ lastStr

getStrings :: [Delta] -> Maybe String
getStrings ds = do
  let initCode = "import Control.Monad.State\nimport qualified Library.EmulateTM as E\n\n"
      lastCode = "qfin = do\n  E.checkBlank\n  return()\n\nmain :: IO ()\nmain = E.callMain q0\n"
  codeStrs <- forM ds makeFunction
  return $ initCode ++ (concat codeStrs) ++ lastCode

main :: IO ()
main = do
  putStrLn "Input the name of the Turing Machine File.  >>"
  fileName <- getLine
  contents <- readFile fileName
  let convertedCode = getStrings =<< concatDelta =<< forM(map (words.getDelta') .init . splitBySemicolon . removeSpace . removeLn $ contents) (convertMult <=< getDelta)
      hsFileName = ((init . init . init) $ fileName) ++ ".hs"
  case convertedCode of Just code -> writeFile hsFileName code
                        Nothing -> putStrLn "## ERROR CAUSED : PATTERN MATCH FAILED. ##\nReview your .tm file and correct the tape-symbol."
