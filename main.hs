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
    t <- getCurrentTime
    setProp e "innerHTML" $ do
      printf
        "<div id=\"alert-%d\" class=\"alert alert-info fade in\" role=\"alert\">\
        \  <button type=\"button\" class=\"close\" data-dismiss=\"alert\"><span aria-hidden=\"true\">&times;</span><span class=\"sr-only\">Close</span></button>%s \
        \</div>" t s
    setTimeout 5000 $ do
      eval $ toJSString $ "$('#alert-" ++ show t ++ "').alert('close')"
      return ()

removeAttr :: (MonadIO m) => Elem -> String -> m ()
removeAttr e c = liftIO $ rmc e c
  where
    {-# NOINLINE rmc #-}
    rmc :: Elem -> String -> IO ()
    rmc = ffi "(function(e,c){e.removeAttribute(c);})"

def :: IO Aichan
def = (\t -> Aichan 0 0 0 0 0 False M.empty M.empty & lastFocus .~ t) <$> getCurrentTime

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
  _achieves :: M.IntMap (Maybe String),
  _items :: M.IntMap Int
  } deriving (Eq, Show)

instance Serialize Aichan where
  toJSON (Aichan ls lp d f _ _ as is) = Dict $ [("loves",toJSON ls),("lps",toJSON lp),("depend",toJSON d),("lastFocus",toJSON $ show f),("achievements",toJSON $ M.assocs as),("items",toJSON $ M.assocs is)]
  parseJSON z = do
    ls <- z .: "loves"
    lp <- z .: "lps"
    d <- z .: "depend"
    f <- read <$> z .: "lastFocus"
    as <- M.fromList <$> z .: "achievements"
    is <- M.fromList <$> z .: "items"
    return $ Aichan ls lp d f 0 False as is

-- makeLenses
loves :: Lens' Aichan Double; loves = lens _loves (\p x -> p { _loves = x })
lps :: Lens' Aichan Double; lps = lens _lps (\p x -> p { _lps = x })
depend :: Lens' Aichan Double; depend = lens _depend (\p x -> p { _depend = x })
lastFocus :: Lens' Aichan Integer; lastFocus = lens _lastFocus (\p x -> p { _lastFocus = x })
interval :: Lens' Aichan Integer; interval = lens _interval (\p x -> p { _interval = x })
hasFocus :: Lens' Aichan Bool; hasFocus = lens _hasFocus (\p x -> p { _hasFocus = x })
achieves :: Lens' Aichan (M.IntMap (Maybe String)); achieves = lens _achieves (\p x -> p { _achieves = x })
items :: Lens' Aichan (M.IntMap Int); items = lens _items (\p x -> p { _items = x })

achievements :: M.IntMap (String, Int -> StateT Aichan IO ())
achievements = M.fromList $ achievementList

achievementList :: [(Int,(String, Int -> StateT Aichan IO ()))]
achievementList = zip [1..] [
  pair "アイとの遭遇" $ loveOver 1,
  pair "愛され気分" $ loveOver 100,
  pair "愛ラブユー" $ loveOver 10000,
  pair "愛さんの愛が重い" $ loveOver 10000000,

  pair "まずはお友達から" $ lpsOver 1,
  pair "二級フラグ建築士" $ lpsOver 5,
  pair "一級フラグ建築士" $ lpsOver 10,
  pair "大好きﾋﾞｰﾑ" $ lpsOver 50,
  pair "ﾊｲﾊﾟｰ大好きﾋﾞｰﾑ" $ lpsOver 100,
  pair "全開らぶぱわー" $ lpsOver 250,

  pair "multiplier 1" $ dependOver 1,
  pair "multiplier 10" $ dependOver 10,
  pair "依存注意報" $ dependOver 100,
  pair "依存ドラッグ" $ dependOver 1000
  ]
  where
    pair s m = (s,m s)
  
    loveOver n s i = do
      m <- use achieves
      lv <- use loves
      when (lv>fromInteger n) $ do
        achieves %= M.insert i (Just $ "愛情が"++show n++"を超える")
        lift $ putAlert $ "実績獲得： " ++ s
        displayTable
    lpsOver n s i = do
      m <- use achieves
      ls <- use lps
      when (ls>fromInteger n) $ do
        achieves %= M.insert i (Just $ "好感度が"++show n++"を超える")
        lift $ putAlert $ "実績獲得： " ++ s
        displayTable
    dependOver n s i = do
      m <- use achieves
      d <- use depend
      when (d>fromInteger n) $ do
        achieves %= M.insert i (Just $ "依存度が"++show n++"を超える")
        lift $ putAlert $ "実績獲得： " ++ s
        displayTable

shopItemList :: [(Int,(Int -> Integer,StateT Aichan IO ()))]
shopItemList = normal ++ special where
  normal = zip [1..] $ [
    (cost 1,booster 0.1),
    (cost 100,booster 1),
    (cost 1000,booster 5),
    (cost 20000,booster 15),
    (cost 200000,booster 50),
    (cost 5000000,booster 100)]
    where
      cost b n = floor $ b * 1.5^n
      booster n = lps += n

  special = zip [-1,-2..] $ [
    (const 10,reset)]
    where
      reset = do
        d <- liftIO def
        as <- use achieves
        put $ d & achieves .~ as
        displayTable

shopItems :: M.IntMap (Int -> Integer,StateT Aichan IO ())
shopItems = M.fromList shopItemList

humanize :: Double -> String
humanize n = if n <= 1000 then sepComma $ take 5 $ printf "%.2f" n else sepComma $ show $ floor n

sepComma :: String -> String
sepComma x = let (a,b) = break (=='.') x in
  (reverse $ concat $ intersperse "," $ chunksOf 3 $ reverse a) ++ b
  where
    chunksOf n xs = let (a,b) = splitAt n xs in
      if b == [] then [a] else a : chunksOf n b

display :: StateT Aichan IO ()
display = do
  go "lps" =<< humanize <$> use lps
  go "loves" =<< humanize <$> use loves
  go "depend" =<< humanize <$> use depend

  where
    go :: String -> String -> StateT Aichan IO ()
    go x u = do
      Just e <- elemById x
      setProp e "innerHTML" u

displayShop :: StateT Aichan IO ()
displayShop = do
  lvs <- use loves
  im <- use items
  forM_ shopItemList $ \(i,(c,m)) -> do
    let eid = if i>0 then "item-" ++ show i else "item-sp-" ++ show (abs i)
    let num = maybe 0 id (M.lookup i im)

    when (i>0) $ do
      withElem (eid ++ "-box") $ \ebox -> do
        Just e <- elemById (eid ++ "-icon")
        i <- getProp e "innerHTML"
        setProp ebox "innerHTML" $ concat $ replicate num i

      withElem (eid ++ "-num") $ \enum -> do
        setProp enum "innerHTML" $ show num

    withElem (eid ++ "-btn") $ \ebtn -> do
      case fromIntegral (c num) <= lvs of
        True -> removeAttr ebtn "disabled"
        otherwise -> setAttr ebtn "disabled" "disabled"

    withElem (eid ++ "-cost") $ \ecost -> do
      setProp ecost "innerHTML" $ sepComma $ show $ c num

displayTable :: StateT Aichan IO ()
displayTable = do
  as <- use achieves
  Just t <- elemById "achievements"
  setProp t "innerHTML" $ ("<thead><tr><th>実績名</th><th>内容</th></tr></thead>"++) $ (\s -> "<tbody>" ++ s ++ "</tbody>") $ concatMap
    (\(a,b) -> printf "<tr><td>%s</td><td>%s</td></tr>" a b)
    (m as)
  where
    m as = fmap (f as) achievementList
    f as (i,(s,_)) = case M.lookup i as of
      Just (Just x) -> (s,x)
      Just Nothing -> (s,"")
      Nothing -> (s,"")

update :: StateT Aichan IO ()
update = do
  ai <- get
  loves += (ai^.lps)/fps
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
      depend %= (\x -> (x-p) `max` 0)
      lps += p
  hasFocus .= f'
  achievesCheck
  where
    achievesCheck = do
      forM_ achievementList $ \(i,(s,m)) -> do
        as <- use achieves
        when (M.notMember i as || as M.! i == Nothing) $ m i

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
    display
    displayShop
  print q
  
  q1 <- loopStateT (floor $ 1000/titleFps) ref $ do
    lift . setTitle =<< printf "%.2f" <$> use loves
    b <- use hasFocus
    when b $ do
      now <- lift getCurrentTime
      old <- use lastFocus
      lastFocus .= now
      i <- use interval
      interval .= (now-old)
  print q1

  q2 <- loopStateT (saveInterval * 1000) ref $ do
    lift . setItem saveName =<< get
    displayTable
  print q2
  
  --withElem "reset" $ \e -> do
  --  onEvent e OnClick $ \_ _ -> do
  --    writeIORef ref =<< def
  --    writeIORef ref =<< execStateT displayTable =<< readIORef ref
  btnEvents ref [1..]
  btnEvents ref [-1,-2..]

btnEvents :: IORef Aichan -> [Int] -> IO ()
btnEvents ref (i:is) = do
  ei <- if i>0
    then elemById $ "item-" ++ show i ++ "-btn"
    else elemById $ "item-sp-" ++ show (abs i) ++ "-btn"
  case ei of
    Just ebtn -> do
      onEvent ebtn OnClick $ \_ _ -> do
        refStateT ref $ do
          items %= M.insertWith (+) i 1
          let (c,m) = shopItems M.! i
          num <- (M.! i) <$> use items
          loves -= fromIntegral (c $ num-1)
          m
--          lift $ putAlert $ "アイテムを購入： " ++ s
          displayTable
        writeLog . show =<< readIORef ref
      btnEvents ref is
    Nothing -> return ()

refStateT :: IORef s -> StateT s IO () -> IO ()
refStateT ref m = do
  writeIORef ref =<< execStateT m =<< readIORef ref

loopStateT :: Int -> IORef s -> StateT s IO () -> IO s
loopStateT c ref m = do
  refStateT ref m
  setTimeout c $ void $ loopStateT c ref m
  readIORef ref
