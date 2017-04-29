module Library.EmulateTM
( 
  -- Newtype date constructor
  State,
  -- Data Synonims
  Stack, CE,
  
  -- Basic Functions
  getTaple, callMain,

  -- Functions using State monad
  checkBlank,
  ml, mr, mn, rp,
  returnError
  ) where


import Control.Monad.State

type Stack = (String, String)
type CE = [(Char, Char, Char)]

-- 標準入力をTapleに変換する関数
getTaple :: String -> (String, String)
getTaple x = (fst h, tail $ snd h)
  where h = break (== ' ') x

-- 標準関数の再定義
addBlank' :: String -> String
addBlank' x = case x of ('B':ys) -> ('B':ys)
                        (y:ys) -> ('B':y:ys)
                        [] -> "B"
                          
reduceBlank' :: String -> String
reduceBlank' []  = []
reduceBlank' [x] = [x]
reduceBlank' (x:y:xs) = if x == 'B' && y == 'B' then reduceBlank' ('B' : xs)
                                                else x : reduceBlank' (y:xs)

-- Blankの管理
addBlank :: State Stack ()
addBlank = do
  (sFst, sSnd) <- get
  let newFst = addBlank' sFst
      newSnd = reverse $ addBlank' $ reverse sSnd
  put (newFst, newSnd)
  return ()

reduceBlank :: State Stack ()
reduceBlank = do
  (sFst, sSnd) <- get
  let newFst = reduceBlank' sFst
      newSnd = reverse $ reduceBlank' $ reverse sSnd
  put (newFst, newSnd)
  return ()

-- Blankのチェック
checkBlank :: State Stack ()
checkBlank = do
  reduceBlank
  addBlank
  return ()

-- テープヘッドの移動
mr :: State Stack ()
mr = do
  (sPrim, sSec) <- get
  let prim = sPrim ++ [head sSec]
      sec  = tail sSec
      val  = head sec
  put (prim, sec)
  addBlank
  return ()

ml :: State Stack ()
ml = do
  (sPrim, sSec) <- get 
  let prim = init sPrim
      val  = last sPrim
      sec  = val : sSec
  put (prim, sec)
  addBlank
  return ()

mn :: State Stack ()
mn = return ()

-- テープヘッドが指す位置の変換
rp :: Char -> State Stack ()
rp c = do
  (sPrim, sSec) <- get
  let newSSec = c : tail sSec
  put (sPrim, newSSec)
  return ()

-- エラー処理
returnError :: State Stack ()
returnError = put ("## ERROR", "CAUSED : PATTERN MATCH FAILED. ##\nReview your .tm file and correct the tape-symbol.")

-- メイン処理
callMain :: State Stack () -> IO ()
callMain q0 = do
  getStr <- fmap getTaple getLine
  let ans = snd $ runState q0 getStr
  putStrLn $ fst ans ++ " " ++ (snd ans)
