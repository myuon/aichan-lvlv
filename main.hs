{-# LANGUAGE ImpredicativeTypes, OverloadedStrings #-}
import Haste
import Haste.Foreign
import Haste.Serialize
import Haste.JSON
import Haste.LocalStorage
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.List
import Data.IORef
import qualified Data.IntMap as M
import Text.Printf
import Lens

fps = 30.0
titleFps = 1
saveInterval = 60
saveName = "Aichan"

setTitle :: String -> IO ()
setTitle t = void $ eval $ toJSString $ "document.title = " ++ show t

getCurrentTime :: IO Integer
getCurrentTime = read . fromJSStr <$> eval "Date.now()"

putAlert :: String -> IO ()
putAlert s = do
  withElem "alerts" $ \e -> do
    k <- getProp e "innerHTML"
    t <- getCurrentTime
    setProp e "innerHTML" $ do
      printf
        "<div id=\"alert-%d\" class=\"alert alert-info fade in\" role=\"alert\">\
        \  <button type=\"button\" class=\"close\" data-dismiss=\"alert\"><span aria-hidden=\"true\">&times;</span><span class=\"sr-only\">Close</span></button>%s</div>\
        \%s" t s k
    eval $ toJSString $ "$('#alert-" ++ show t ++ "').animate({ \
      \ top: \"50px\" \
    \ })"
    setTimeout 5000 $ do
      eval $ toJSString $ "$('#alert-" ++ show t ++ "').alert('close')"
      return ()

def :: IO Aichan
def = (\t -> Aichan 0 0 0 0 0 False M.empty & lastFocus .~ t) <$> getCurrentTime

docFocused :: IO Bool
docFocused = do
  f <- eval "document.hasFocus()"
  return $ case f of
    "true" -> True
    "false" -> False
    z -> error $ "docFocused: " ++ show z

data Aichan = Aichan {
  _loves :: Double,
  _lps :: Double,
  _depend :: Double,
  _lastFocus :: Integer,
  _interval :: Integer,
  _hasFocus :: Bool,
  _achieves :: M.IntMap (String, String)
  } deriving (Eq, Show)

instance Serialize Aichan where
  toJSON (Aichan ls lp d f _ _ as) = Dict $ [("loves",toJSON ls),("lps",toJSON lp),("depend",toJSON d),("lastFocus",toJSON $ show f),("achievements",toJSON $ M.assocs as)]
  parseJSON z = do
    ls <- z .: "loves"
    lp <- z .: "lps"
    d <- z .: "depend"
    f <- read <$> z .: "lastFocus"
    as <- M.fromList <$> z .: "achievements"
    return $ Aichan ls lp d f 0 False as

-- makeLenses
loves :: Lens' Aichan Double; loves = lens _loves (\p x -> p { _loves = x })
lps :: Lens' Aichan Double; lps = lens _lps (\p x -> p { _lps = x })
depend :: Lens' Aichan Double; depend = lens _depend (\p x -> p { _depend = x })
lastFocus :: Lens' Aichan Integer; lastFocus = lens _lastFocus (\p x -> p { _lastFocus = x })
interval :: Lens' Aichan Integer; interval = lens _interval (\p x -> p { _interval = x })
hasFocus :: Lens' Aichan Bool; hasFocus = lens _hasFocus (\p x -> p { _hasFocus = x })
achieves :: Lens' Aichan (M.IntMap (String, String)); achieves = lens _achieves (\p x -> p { _achieves = x })

humanize :: Double -> String
humanize n = if n >= 100 then h' $ show $ floor n else h' $ printf "%.2f" n where
  h' x = let (a,b) = break (=='.') x in
    (reverse $ concat $ intersperse "," $ chunksOf 3 $ reverse a) ++ b

  chunksOf n xs = let (a,b) = splitAt n xs in
    if b == [] then [a] else a : chunksOf n b

display :: StateT Aichan IO ()
display = do
  go "lps" =<< humanize <$> use lps
  go "loves" =<< humanize <$> use loves
  go "depend" =<< humanize <$> use depend
  go "interval" =<< humanize . fromIntegral <$> use interval

  where
    go :: String -> String -> StateT Aichan IO ()
    go x u = do
      Just e <- elemById x
      setProp e "innerHTML" u

displayTable :: StateT Aichan IO ()
displayTable = do
  m <- use achieves
  Just t <- elemById "achievements"
  setProp t "innerHTML" $ concatMap
    (\(_,(a,b)) -> printf "<tr><td>%s</td><td>%s</td></tr>" a b)
    ((0,("実績名","内容")) : M.assocs m)

update :: StateT Aichan IO ()
update = do
  ai <- get
  loves += (ai^.lps)*(ai^.depend)/fps
  f' <- lift docFocused
  case f' of
    True -> do
      depend += (fromIntegral (ai^.interval)/1000/60/15)
      f <- use hasFocus
      when (f == False) $ do
        k <- use interval
        lift $ putAlert $ "放置期間 +" ++ show k
    False -> do
      let p = fromIntegral (ai^.interval)/1000/60/120
      depend %= (\x -> (x-p) `max` 1)
      lps += p
  hasFocus .= f'

updateAchieves :: StateT Aichan IO ()
updateAchieves = do
  loveOver 1 1 "アイとの遭遇"
  loveOver 12 100 "愛され気分"
  loveOver 13 10000 "愛ラブユー"
  loveOver 14 10000000 "愛さんの愛が重い"
  
  lpsOver 21 1 "まずはお友達から"
  lpsOver 22 5 "二級フラグ建築士"
  lpsOver 23 10 "一級フラグ建築士"
  lpsOver 24 50 "大好きﾋﾞｰﾑ"
  lpsOver 25 200 "全開らぶぱわー"
  
  dependOver 31 1 "multiplier 1.0"
  dependOver 32 10 "multiplier 10.0"
  dependOver 33 100 "依存注意報"
  dependOver 34 1000 "依存ドラッグ"
  where
    loveOver i n s = do
      m <- use achieves
      lv <- use loves
      when (M.member i m == False && lv>n) $ do
        achieves %= M.insert i (s,"愛情が"++show n++"を超える")
        lift $ putAlert $ "実績獲得： " ++ s
        displayTable

    lpsOver i n s = do
      m <- use achieves
      lv <- use lps
      when (M.member i m == False && lv>n) $ do
        achieves %= M.insert i (s,"好感度が"++show n++"を超える")
        lift $ putAlert $ "実績獲得： " ++ s
        displayTable

    dependOver i n s = do
      m <- use achieves
      lv <- use depend
      when (M.member i m == False && lv>n) $ do
        achieves %= M.insert i (s,"依存度が"++show n++"を超える")
        lift $ putAlert $ "実績獲得： " ++ s
        displayTable

loadItem :: (Serialize a) => String -> a -> IO (IORef a)
loadItem s a = do
  g <- getItem saveName
  newIORef $ case g of
    Left _ -> a
    Right r -> r

main = do
  ref <- loadItem saveName =<< def
  q <- loopStateT (floor $ 1000/fps) ref $ do
    update
    updateAchieves
    display
  print q
  
  q1 <- loopStateT (floor $ 1000/titleFps) ref $ do
    lift . setTitle =<< printf "%.2f" <$> use loves
    b <- use hasFocus
    when b $ do
      now <- lift getCurrentTime
      old <- use lastFocus
      lastFocus .= now
      i <- use interval
      interval .= div (now-old+i*2) 3
  print q1

  q2 <- loopStateT (saveInterval * 1000) ref $ do
    lift . setItem saveName =<< get
    displayTable
  print q2
  
  Just e <- elemById "reset"
  onEvent e OnClick $ \_ _ -> do
    writeIORef ref =<< def
    writeIORef ref =<< execStateT displayTable =<< readIORef ref

loopStateT :: Int -> IORef s -> StateT s IO () -> IO s
loopStateT c ref m = do
  writeIORef ref =<< execStateT m =<< readIORef ref
  setTimeout c $ void $ loopStateT c ref m
  readIORef ref

