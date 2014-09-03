{-# LANGUAGE ImpredicativeTypes #-}
import Haste
import Haste.Foreign
import Haste.Serialize
import Haste.JSON
import Haste.LocalStorage
import qualified Haste.Perch as P
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.List
import Data.IORef
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Text.Printf
import Numeric

import Lens

fps = 30.0
titleFps = 1
saveInterval = 60
saveName = "Aichan"

setTitle :: String -> IO ()
setTitle t = void $ eval $ toJSString $ "document.title = " ++ show t

getCurrentTime :: IO Integer
getCurrentTime = read . fromJSStr <$> (eval . toJSString) "Date.now()"

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
    rmc = ffi $ toJSString "(function(e,c){e.removeAttribute(c);})"

def :: IO Aichan
def = (\t -> Aichan 0 0 0 0 False IM.empty IM.empty 0 & lastFocus .~ t) <$> getCurrentTime

docFocused :: IO Bool
docFocused = do
  f <- eval $ toJSString "document.hasFocus()"
  return $ case show f of
    "true" -> True
    "false" -> False
    z -> error $ "docFocused: " ++ show z

data Aichan = Aichan {
  _loves :: Double,
  _lps :: Double,
  _depend :: Double,
  _lastFocus :: Integer,
  _hasFocus :: Bool,
  _achieves :: IM.IntMap (Maybe String),
  _items :: IM.IntMap Int,

  _maxLoves :: Double
  } deriving (Eq, Show)

instance Serialize Aichan where
  toJSON (Aichan ls lp d f _ as is ml) = Dict $ fmap (\(x,y) -> (toJSString x,y)) $
    [("loves",toJSON ls),("lps",toJSON lp),("depend",toJSON d),("lastFocus",toJSON $ show f),
     ("achievements",toJSON $ IM.assocs as),("items",toJSON $ IM.assocs is),
     ("maxLoves",toJSON ml)]
  parseJSON z = do
    ls <- z .: toJSString "loves"
    lp <- z .: toJSString "lps"
    d <- z .: toJSString "depend"
    f <- read <$> z .: toJSString "lastFocus"
    as <- IM.fromList <$> z .: toJSString "achievements"
    is <- IM.fromList <$> z .: toJSString "items"
    ml <- z .: toJSString "maxLoves"
    return $ Aichan ls lp d f False as is ml

-- makeLenses
loves :: Lens' Aichan Double; loves = lens _loves (\p x -> p { _loves = x })
lps :: Lens' Aichan Double; lps = lens _lps (\p x -> p { _lps = x })
depend :: Lens' Aichan Double; depend = lens _depend (\p x -> p { _depend = x })
lastFocus :: Lens' Aichan Integer; lastFocus = lens _lastFocus (\p x -> p { _lastFocus = x })
hasFocus :: Lens' Aichan Bool; hasFocus = lens _hasFocus (\p x -> p { _hasFocus = x })
achieves :: Lens' Aichan (IM.IntMap (Maybe String)); achieves = lens _achieves (\p x -> p { _achieves = x })
items :: Lens' Aichan (IM.IntMap Int); items = lens _items (\p x -> p { _items = x })
maxLoves :: Lens' Aichan Double; maxLoves = lens _maxLoves (\p x -> p { _maxLoves = x })

achievements :: IM.IntMap (String, Int -> StateT Aichan IO ())
achievements = IM.fromList $ achievementList

achievementList :: [(Int,(String, Int -> StateT Aichan IO ()))]
achievementList = zip [1..] [
  pair "アイとの遭遇" $ loveOver 100,
  pair "愛され気分" $ loveOver 10000,
  pair "愛ラブユー" $ loveOver 1000000,
  pair "愛さんの愛が重い" $ loveOver 100000000,

  pair "まずはお友達から" $ lpsOver 10,
  pair "一級フラグ建築士" $ lpsOver 100,
  pair "大好きﾋﾞｰﾑ" $ lpsOver 500,
  pair "ﾊｲﾊﾟｰ大好きﾋﾞｰﾑ" $ lpsOver 1000,
  pair "全開らぶぱわー" $ lpsOver 10000,

  pair "依存注意報" $ dependOver 100,
  pair "依存ドラッグ" $ dependOver 1000,

  pair "コンプリート" $ itemAllOver 1,
  pair "ココココココココココンプリート" $ itemAllOver 10
  ]
  where
    pair s m = (s,m s)
    achieve d s i = do
      achieves %= IM.insert i (Just d)
      lift $ putAlert $ "実績獲得: " ++ s
      save

    itemAllOver n s i = do
      im <- use items
      let b = all (\x -> IM.member x im && im IM.! x >= n) $ IM.keys $ IM.filterWithKey (\k _ -> k > 0) $ shopItems
      when b $ achieve ("全ての通常アイテムを" ++ show n ++ "個以上手に入れる") s i
    loveOver n s i = do
      m <- use achieves
      lv <- use loves
      when (lv>fromInteger n) $ achieve ("愛情が"++show n++"を超える") s i
    lpsOver n s i = do
      m <- use achieves
      ls <- use lps
      when (ls>fromInteger n) $ achieve ("好感度が"++show n++"を超える") s i
    dependOver n s i = do
      m <- use achieves
      d <- use depend
      when (d>fromInteger n) $ achieve ("依存度が"++show n++"を超える") s i

type Item = (Int -> Integer,Int -> StateT Aichan IO (),(String,String,String))

spItemNames :: M.Map String Int
spItemNames = M.fromList $ zip names [-1,-2..] where
  names = ["monitor","item-shop","reset","resetAll"]

shopItemList :: [(Int,Item)]
shopItemList = normal ++ special where
  normal = zip [1..] $ [
    (cost 1,booster 0.2,("fa-comments-o","会話<br>好感度 +0.2","")),
    (cost 50,booster 1,("fa-envelope","メール<br>好感度 +1.0","")),
    (cost 1000,booster 10,("fa-coffee","喫茶店<br>好感度 +10","")),
    (cost 20000,booster 40,("fa-plane","旅行<br>好感度 +40","")),
    (cost 500000,booster 100,("fa-car","車<br>好感度 +100","")),
    (cost 10000000,booster 500,("fa-home","家<br>好感度 +500.0",""))]
    where
      cost b n = floor $ b * 1.5^n
      booster n _ = lps += n

  special = zip [-1,-2..] $ [
    (const 0,disp "monitor",("fa-power-off","さぁ始めよう<br>ゲームを始めましょう。右のボタンからこのアイテムを購入してください。","さぁ始めよう")),
    (const 1,disp "item-shop",("fa-shopping-cart","アイテムショップ<br>アイテムが購入できるようになります。","アイテムショップ")),
    (const 10,reset,("fa-history","初期化<br>実績を除く全てのデータが初期化されます","初期化")),
    (const 100,resetAll,("fa-trash","データの消去<br>全てのデータが消去されます。この操作は取り消せません。","データの消去"))]
    where
      disp k i = do
        withElem k $ \e -> setStyle e "display" "block"
        when (k == "monitor") $ lastFocus <~ liftIO getCurrentTime
      resetAll _ = do
        put =<< liftIO def
        save
        withElem "monitor" $ \e -> setStyle e "display" "none"
        withElem "item-shop" $ \e -> setStyle e "display" "none"
      reset _ = do
        d <- liftIO def
        as <- use achieves
        put $ d & achieves .~ as
        save
        withElem "monitor" $ \e -> setStyle e "display" "none"
        withElem "item-shop" $ \e -> setStyle e "display" "none"

shopItems :: IM.IntMap Item
shopItems = IM.fromList shopItemList

itemLi :: Int -> String -> String -> String -> P.Perch
itemLi i icon tips box = do
  let p = "item-" ++ (if i>0 then show i else "sp-" ++ show (abs i))
  P.li P.! P.id p P.! P.atr "class" "list-group-item tooltips" $ do
    P.span P.! P.atr "class" "tip" $ do
      P.setHtml P.this tips
    P.span P.! P.atr "class" "count" $ do
      P.span P.! P.atr "id" (p ++ "-icon") $ do
        P.i P.! P.atr "class" ("fa " ++ icon) $ ""
        P.toElem " "
      P.span P.! P.atr "id" (p ++ "-num") $ ""

    P.span P.! P.atr "id" (p ++ "-box") P.! P.atr "class" "item-list" $ box

    P.button P.! P.atr "type" "button" P.! P.atr "id" (p ++ "-btn") P.! P.atr "class" "btn btn-default btn-buy" $ do
      P.i P.! P.atr "class" "fa fa-plus-circle" $ ""
      P.toElem " "
      P.span P.! P.atr "id" (p ++ "-cost") $ "0"
      P.toElem " loves"

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
  forM_ shopItemList $ \(i,(c,m,_)) -> do
    let eid = if i>0 then "item-" ++ show i else "item-sp-" ++ show (abs i)
    let num = maybe 0 id (IM.lookup i im)

    when (i>0) $ do
      withElem (eid ++ "-box") $ \ebox -> do
        Just e <- elemById (eid ++ "-icon")
        i <- getProp e "innerHTML"
        let cs = [500,100,50,10,5,1]
        let ds = zip cs $ reverse $ snd $ foldl' (\(n,as) a -> (n`mod`a,(n`div`a) : as)) (num,[]) cs
        setProp ebox "innerHTML" $
          foldl' (\b (n,k) -> printf "%s<span class=\"item-%d\">%s</span>" b n $ concat $ replicate k i) "" ds

      withElem (eid ++ "-num") $ \enum -> do
        setProp enum "innerHTML" $ show num

    let cond = if i>0 then True else IM.notMember i im || im IM.! i < 1
    withElem (eid ++ "-btn") $ \ebtn -> do
      case cond && fromIntegral (c num) <= lvs of
        True -> removeAttr ebtn "disabled"
        otherwise -> setAttr ebtn "disabled" "disabled"

    withElem (eid ++ "-cost") $ \ecost -> do
      setProp ecost "innerHTML" $ sepComma $ show $ c num

    ml <- use maxLoves
    when (IM.member i im || (fromIntegral $ c 0) <= 3*ml) $ do
      withElem eid $ \k -> do
        a <- getStyle k "display"
        when (a /= "block") $ setStyle k "display" "block"

displayTable :: StateT Aichan IO ()
displayTable = do
  as <- use achieves
  Just t <- elemById "achievements"
  setProp t "innerHTML" $ ("<thead><tr><th>実績名</th><th>内容</th></tr></thead>"++) $ (\s -> "<tbody>" ++ s ++ "</tbody>") $ concatMap
    (\(a,b) -> printf "<tr><td>%s</td><td>%s</td></tr>" a b)
    (m as)
  where
    m as = fmap (f as) achievementList
    f as (i,(s,_)) = case IM.lookup i as of
      Just (Just x) -> (s,x)
      Just Nothing -> (s,"")
      Nothing -> (s,"")

update :: StateT Aichan IO ()
update = do
  ai <- get
  loves += (ai^.lps)/fps
  f' <- lift docFocused
  now <- liftIO getCurrentTime
  let diff = fromIntegral $ now - (ai^.lastFocus)

  case f' of
    True -> do
      f <- use hasFocus
      when (f == False) $ do
        let bonus = diff/1000/50
        depend += bonus
        lift $ putAlert $ "おかえりボーナス<br>依存度 +" ++ humanize bonus

      depend += (diff/1000/1000)
      lps += (diff/1000/100)

      lastFocus .= now

    False -> do
      let p = 0.01
      depend %= (\x -> (x-p) `max` 0)
      when ((ai^.depend) > 0) $
        lps += p
  hasFocus .= f'
  achievesCheck
  maxCheck

  where
    achievesCheck = do
      forM_ achievementList $ \(i,(s,m)) -> do
        as <- use achieves
        when (IM.notMember i as || as IM.! i == Nothing) $ m i

    maxCheck = do
      ml <- use maxLoves
      lv <- use loves
      when (lv > ml) $ maxLoves .= lv

loadItem :: (Serialize a) => String -> a -> IO (IORef a)
loadItem s a = do
  g <- getItem saveName
  newIORef $ case g of
    Left _ -> a
    Right r -> r

save :: StateT Aichan IO ()
save = do
  lift . setItem saveName =<< get
  displayTable

main = do
  ref <- loadItem saveName =<< def
  let (a,b) = partition ((>0) . fst) shopItemList
  withElem "list-group" $ \e -> do
    forM_ a $ \(i,(_,_,(icon,tip,box))) -> do
      P.build (itemLi i icon tip box) e
  withElem "list-sp-group" $ \e -> do
    forM_ b $ \(i,(_,_,(icon,tip,box))) -> do
      P.build (itemLi i icon tip box) e

  btnEvents ref [1..]
  btnEvents ref [-1,-2..]

  let notRecover = ["reset","resetAll"]
  let rec = IM.filterWithKey (\k _ -> k `notElem` fmap (spItemNames M.!) notRecover) shopItems
  forM_ (IM.assocs rec) $ \(i,t) -> do
    let (_,m,_) = t
    refStateT ref $ do
      im <- use items
      when (IM.member i im && im IM.! i > 0) $ m i

  q <- loopStateT (floor $ 1000/fps) ref $ do
    im <- use items
    when (IM.member (spItemNames M.! "monitor") im) update
    display
    displayShop
  print q
  
  q1 <- loopStateT (floor $ 1000/titleFps) ref $ do
    lift . setTitle =<< printf "%.2f" <$> use loves
  print q1

  q2 <- loopStateT (saveInterval * 1000) ref $ do
    save
    liftIO $ putAlert "セーブしました"
  print q2
  
btnEvents :: IORef Aichan -> [Int] -> IO ()
btnEvents ref (i:is) = do
  ei <- if i>0
    then elemById $ "item-" ++ show i ++ "-btn"
    else elemById $ "item-sp-" ++ show (abs i) ++ "-btn"
  case ei of
    Just ebtn -> do
      onEvent ebtn OnClick $ \_ _ -> do
        refStateT ref $ do
          items %= IM.insertWith (+) i 1
          let (c,m,_) = shopItems IM.! i
          num <- (IM.! i) <$> use items
          loves -= fromIntegral (c $ num-1)
          m i
          save
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
