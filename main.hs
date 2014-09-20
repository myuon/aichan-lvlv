{-# LANGUAGE ImpredicativeTypes #-}
import Haste
import Haste.Foreign
import Haste.Serialize
import Haste.JSON
import Haste.LocalStorage
import qualified Haste.Binary as B
import qualified Haste.Perch as P
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.List
import Data.IORef
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Text.Printf
import Numeric
import System.IO.Unsafe (unsafePerformIO)

import Lens

fps = 30.0
titleFps = 1
saveInterval = 60
saveName = "Aichan"

setTitle :: String -> IO ()
setTitle t = void $ eval $ toJSString $ "document.title = " ++ show t

getCurrentTime :: IO Integer
getCurrentTime = read . fromJSStr <$> (eval . toJSString) "Date.now()"

getCurrentPrettyTime :: IO String
getCurrentPrettyTime = fromJSStr <$> (eval . toJSString)
  "var d = new Date(); d.getHours() + ':' + d.getMinutes() + ':' + d.getSeconds()"

removeAttr :: (MonadIO m) => Elem -> String -> m ()
removeAttr e c = liftIO $ rmc e c
  where
    {-# NOINLINE rmc #-}
    rmc :: Elem -> String -> IO ()
    rmc = ffi $ toJSString "(function(e,c){e.removeAttribute(c);})"

addChildFirst :: (MonadIO m) => Elem -> Elem -> m ()
addChildFirst e c = liftIO $ acf e c
  where
    {-# NOINLINE acf #-}
    acf :: Elem -> Elem -> IO ()
    acf = ffi $ toJSString "(function(e,c){var first = e.firstChild; e.insertBefore(c,first);})"

putAlert :: String -> String -> StateT Aichan IO ()
putAlert person s = do
  t <- liftIO getCurrentTime
  liftIO $ withElem "alerts" $ P.build $ do
    P.setHtml P.this ""
    P.div P.! P.id ("alert-" ++ show t) P.! P.atr "class" ("alert alert-info fade in tip") P.! P.atr "role" "alert" $ do
      P.button P.! P.atr "type" "button" P.! P.atr "class" "close" P.! P.atr "data-dismiss" "alert" $ do
        P.span P.! P.atr "aria-hidden" "true" $ P.setHtml P.this "&times;"
        P.span P.! P.atr "class" "sr-only" $ "Close"
      P.span $ do
        P.setHtml P.this s

  liftIO $ setTimeout 5000 $ do
    eval $ toJSString $ "$('#alert-" ++ show t ++ "').alert('close')"
    return ()
  putLog person s

putLogIO :: String -> String -> Int -> IO ()
putLogIO person s num = do
  withElem "log-group" $ \e -> do
    stamp <- liftIO getCurrentPrettyTime
    li <- newElem "li"
    addChildFirst e =<< P.build (listGroupItem stamp) li

  withElem "unread-badge" $ \e -> do
    setProp e "innerHTML" $ show num

  return ()
  where
    listGroupItem stamp = do
      P.this P.! P.atr "class" "list-group-item"
      P.child P.this $ do
        P.strong $ person ++ " (" ++ stamp ++ ") "
        P.span $ P.setHtml P.this $ replace "<br>" " " s

    replace _ _ [] = []
    replace x y str@(s:ss)
      | isPrefixOf x str = y ++ replace x y (drop (length x) str)
      | otherwise = s:replace x y ss

putLog :: String -> String -> StateT Aichan IO ()
putLog person s = do
  logUnread += 1
  lift . putLogIO person s =<< use logUnread

def :: IO Aichan
def = do
  t <- getCurrentTime
  Just elm <- elemById "unread-badge"
  ur <- getProp elm "innerHTML"
  let num = if ur == "" then 0 else read ur

  return $ Aichan 0 0 0 0 False M.empty M.empty 0 1 1 0
    & lastFocus .~ t & logUnread .~ num

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
  _achieves :: M.Map String (Maybe String),
  _items :: M.Map String Int,

  _maxLoves :: Double,
  _dependCoeff :: Double,
  _lpsCoeff :: Double,

  _logUnread :: Int
  } deriving (Eq, Show)

instance Serialize Aichan where
  toJSON (Aichan ls lp d f _ as is ml dc lc _) = Dict $ fmap (\(x,y) -> (toJSString x,y)) $
    [("loves",toJSON ls),("lps",toJSON lp),("depend",toJSON d),("lastFocus",toJSON $ show f),
     ("achievements",toJSON $ M.assocs as),("itemsWithMap",toJSON $ M.assocs is),
     ("maxLoves",toJSON ml),("dependCoeff",toJSON dc),("lpsCoeff",toJSON lc)]
  parseJSON z = do
    ls <- z .: toJSString "loves"
    lp <- z .: toJSString "lps"
    d <- z .: toJSString "depend"
    f <- read <$> z .: toJSString "lastFocus"
    as <- fmap M.fromList <$> z .:? toJSString "achievements"
    is <- fmap M.fromList <$> z .:? toJSString "itemsWithMap"
    ml <- z .: toJSString "maxLoves"
    dc <- z .:? toJSString "dependCoeff"
    lc <- z .:? toJSString "lpsCoeff"

    is' <- z .:? toJSString "items"
    m <- return $ IM.fromList
      [(1,"chat"),(2,"mail"),(3,"coffee"),(4,"gift"),(5,"trip"),(6,"car"),(7,"house"),(8,"righteye"),(9,"lefteye")
      ,(-1,"monitor"),(-2,"itemshop"),(-3,"righteye"),(-4,"lefteye"),(-5,"righteye"),(-6,"lefteye"),(-7,"reset"),(-8,"restart")]
    let oldMap = M.fromList . fmap (\(k,s) -> (m IM.! k,s)) <$> is'

    return $ (unsafePerformIO def) 
      & loves .~ ls & lps .~ lp & depend .~ d & lastFocus .~ f & achieves ?~ as
      & items ?~ (if oldMap /= Nothing then oldMap else is) & maxLoves .~ ml & dependCoeff ?~ dc & lpsCoeff ?~ lc

-- makeLenses
loves :: Lens' Aichan Double; loves = lens _loves (\p x -> p { _loves = x })
lps :: Lens' Aichan Double; lps = lens _lps (\p x -> p { _lps = x })
depend :: Lens' Aichan Double; depend = lens _depend (\p x -> p { _depend = x })
lastFocus :: Lens' Aichan Integer; lastFocus = lens _lastFocus (\p x -> p { _lastFocus = x })
hasFocus :: Lens' Aichan Bool; hasFocus = lens _hasFocus (\p x -> p { _hasFocus = x })
achieves :: Lens' Aichan (M.Map String (Maybe String)); achieves = lens _achieves (\p x -> p { _achieves = x })
items :: Lens' Aichan (M.Map String Int); items = lens _items (\p x -> p { _items = x })
maxLoves :: Lens' Aichan Double; maxLoves = lens _maxLoves (\p x -> p { _maxLoves = x })
dependCoeff :: Lens' Aichan Double; dependCoeff = lens _dependCoeff (\p x -> p { _dependCoeff = x })
lpsCoeff :: Lens' Aichan Double; lpsCoeff = lens _lpsCoeff (\p x -> p { _lpsCoeff = x })
logUnread :: Lens' Aichan Int; logUnread = lens _logUnread (\p x -> p { _logUnread = x })

achievements :: M.Map String (StateT Aichan IO ())
achievements = M.fromList $ achievementList

achievementList :: [(String, StateT Aichan IO ())]
achievementList = [
  pair "アイとの遭遇" $ loveOver 100,
  pair "愛され気分" $ loveOver 10000,
  pair "愛ラブユー" $ loveOver 1000000,
  pair "愛さんの愛が重い" $ loveOver 100000000,
  pair "愛ちゃんマスター" $ loveOver 100000000000,

  pair "まずはお友達から" $ lpsOver 10,
  pair "一級フラグ建築士" $ lpsOver 100,
  pair "大好きﾋﾞｰﾑ" $ lpsOver 1000,
  pair "ﾊｲﾊﾟｰ大好きﾋﾞｰﾑ" $ lpsOver 10000,
  pair "全開らぶぱわー" $ lpsOver 100000,

  pair "依存注意報" $ dependOver 100,
  pair "依存ドラッグ" $ dependOver 10000,

  pair "おしゃべり愛ちゃん" $ itemOver "chat" 100,
  pair "あ、うん" $ itemOver "chat" 200,
  pair "文通相手" $ itemOver "mail" 100,
  pair "喫茶店のポイントカード" $ itemOver "coffee" 100,
  pair "愛という名のプレゼント" $ itemOver "gift" 50,
  pair "マイルがたくさん" $ itemOver "trip" 50,
  pair "車コレクター" $ itemOver "car" 50,
  pair "別荘マニア" $ itemOver "house" 50,

  pair "コンプリート" $ itemAllOver 1,
  pair "ココココココココココンプリート" $ itemAllOver 10,

  pair "長い長いログ" $ logOver 100,

  pair "放置プレイ" $ loveOverWithNoItem 1000000
  ]
  where
    pair s m = (s,m s)
    achieve d s = do
      achieves %= M.insert s (Just d)
      putAlert "game" $ "実績獲得: " ++ s
      save

    loveOverWithNoItem n s = do
      m <- use achieves
      lv <- use loves
      im <- use items
      when (lv>fromInteger n && all (\t -> M.notMember t im || im M.! t == 0) normalItemNames) $ achieve ("通常アイテムを買っていない状態で愛情が"++show n++"を超える") s
    itemOver k n s = do
      im <- use items
      let name = (\(_,_,(_,_,c)) -> c) $ shopItems M.! k
      when (M.member k im && im M.! k >= n) $ achieve ("アイテム「" ++ name ++ "」を" ++ show n ++ "個以上手に入れる") s
    itemAllOver n s = do
      im <- use items
      let b = all (\x -> M.member x im && im M.! x >= n) $ normalItemNames
      when b $ achieve ("全ての通常アイテムを" ++ show n ++ "個以上手に入れる") s
    loveOver n s = do
      m <- use achieves
      lv <- use loves
      when (lv>fromInteger n) $ achieve ("愛情が"++show n++"を超える") s
    lpsOver n s = do
      m <- use achieves
      ls <- use lps
      when (ls>fromInteger n) $ achieve ("好感度が"++show n++"を超える") s
    dependOver n s = do
      m <- use achieves
      d <- use depend
      when (d>fromInteger n) $ achieve ("依存度が"++show n++"を超える") s
    logOver d s = do
      ur <- use logUnread
      when (ur >= d) $
        achieve ("ログの行数が"++show d++"を超える") s

type Item = (Int -> Integer,Int -> StateT Aichan IO (),(String,String,String))

normalItemNames :: [String]
normalItemNames = takeWhile (/= "monitor") $ fmap fst $ shopItemList

shopItemList :: [(String,Item)]
shopItemList = normal ++ special where
  normal = [
    (,) "chat" (cost 1,booster 0.2,("fa-comments-o","会話<br>好感度 +0.2","会話")),
    (,) "mail" (cost 50,booster 1,("fa-envelope","メール<br>好感度 +1.0","メール")),
    (,) "coffee" (cost 1000,booster 10,("fa-coffee","喫茶店<br>好感度 +10","喫茶店")),
    (,) "gift" (cost 20000,booster 100,("fa-gift","プレゼント<br>好感度 +100","プレゼント")),
    (,) "trip" (cost 500000,booster 1000,("fa-plane","旅行<br>好感度 +1000","旅行")),
    (,) "car" (cost 10000000,booster 15000,("fa-car","車<br>好感度 +15000","車")),
    (,) "house" (cost 250000000,booster 200000,("fa-home","家<br>好感度 +200000","家")),
    (,) "righteye" (costMul 3 100000,dependMulti 2,("fa-eye","アイちゃんの右目<br>依存度ボーナスが増えます。","アイちゃんの右目")),
    (,) "lefteye" (costMul 3 100000,lpsMulti 2,("fa-eye","アイちゃんの左目<br>依存度が好感度に変わる速さが速くなります。","アイちゃんの左目"))]
    where
      cost b n = floor $ b * 1.25^n
      costMul r a n = floor $ a * r^n
      booster n _ = lps += n
      dependMulti d n = dependCoeff += d^n
      lpsMulti d n = lpsCoeff += d^n

  special = [
    (,) "monitor" (const 0,disp "monitor",("fa-power-off","さぁ始めよう<br>ゲームを始めましょう。右のボタンからこのアイテムを購入してください。","さぁ始めよう")),
    (,) "itemshop" (const 1,disp "itemshop",("fa-shopping-cart","アイテムショップ<br>アイテムが購入できるようになります。","アイテムショップ")),
    (,) "save" (const 0,doSave,("fa-save","セーブ<br>セーブします。","セーブ")),
    (,) "reset" (const 10,reset,("fa-history","初期化<br>実績を除く全てのデータをリセットします。<br>今ある依存度は好感度係数に、愛情は依存度係数に加わります。","初期化"))]
--    (,) "restart" (const 100,resetAll,("fa-trash","データの消去<br>全てのデータが消去されます。この操作は取り消せません。","データの消去"))]
--    (,) "night" (const 5,night,("fa-moon-o","夜<br>夜が訪れます。<br><strong>※注意 このアイテムの効果は保存されません。ブラウザがリロードされると元に戻ります。</strong>","夜"))]
    where
      disp k _ = do
        withElem k $ \e -> setStyle e "display" "block"
        withElem (printf "item-%s" k) $ \e -> setStyle e "display" "none"
        when (k == "monitor") $ lastFocus <~ liftIO getCurrentTime
      reset _ = do
        d <- liftIO def
        as <- use achieves
        lc <- (\x -> (sqrt x/10^2) * log x) <$> use depend
        dc <- (\x -> (sqrt x/10^4) * log x) <$> use loves
        put $ d & achieves .~ as & dependCoeff +~ dc & lpsCoeff +~ lc
        save
        withElem "monitor" $ \e -> setStyle e "display" "none"
        withElem "itemshop" $ \e -> setStyle e "display" "none"
      doSave _ = do
        items %= M.insert "save" 0
        save
        putAlert "auto" "セーブしました"
      night _ = do
        items %= M.insert "night" 0
        save
        setClass documentBody "night" True

shopItems :: M.Map String Item
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
  forM_ shopItemList $ \(i,(c,m,_)) -> do
    let eid = "item-" ++ i
    let num = maybe 0 id (M.lookup i im)

    when (i `elem` normalItemNames) $ do
      withElem (eid ++ "-box") $ \ebox -> do
        Just e <- elemById (eid ++ "-icon")
        i <- getProp e "innerHTML"
        let cs = [500,100,50,10,5,1]
        let ds = zip cs $ reverse $ snd $ foldl' (\(n,as) a -> (n`mod`a,(n`div`a) : as)) (num,[]) cs
        setProp ebox "innerHTML" $
          foldl' (\b (n,k) -> printf "%s<span class=\"item-%d\">%s</span>" b n $ concat $ replicate k i) "" ds

      withElem (eid ++ "-num") $ \enum -> do
        setProp enum "innerHTML" $ show num

    let cond = if i `elem` normalItemNames then True else M.notMember i im || im M.! i < 1
    withElem (eid ++ "-btn") $ \ebtn -> do
      case cond && fromIntegral (c num) <= lvs of
        True -> removeAttr ebtn "disabled"
        otherwise -> setAttr ebtn "disabled" "disabled"

    withElem (eid ++ "-cost") $ \ecost -> do
      setProp ecost "innerHTML" $ sepComma $ show $ c num

    ml <- use maxLoves
    when (i `elem` normalItemNames && (M.member i im || (fromIntegral $ c 0) <= 3*ml)) $ do
      withElem eid $ \k -> do
        a <- getStyle k "display"
        when (a /= "block") $ setStyle k "display" "block"

displayAchievements :: StateT Aichan IO ()
displayAchievements = do
  as <- use achieves
  qs <- stats
  liftIO $ withElem "stats" $ P.build $ do
    P.setHtml P.this ""
    P.thead $ P.tr $ do
      P.th "項目名"
      P.th "内容"
    P.tbody $ forM_ qs $ \(a,b) -> P.tr $ do
      P.td a
      P.td b

  liftIO $ withElem "achievements" $ P.build $ do
    P.setHtml P.this ""
    P.thead $ P.tr $ do
      P.th "実績名"
      P.th "内容"
    P.tbody $ forM_ (m as) $ \(a,b) -> P.tr $ do
      P.td a
      P.td b
  return ()

  where
    stats = do
      g <- get
      return $ [
        (,) "愛情" (humanize $ g^.loves),
        (,) "好感度(愛情/s)" (humanize $ g^.lps),
        (,) "依存度" (humanize $ g^.depend),
        (,) "好感度係数" (humanize $ g^.lpsCoeff),
        (,) "依存度係数" (humanize $ g^.dependCoeff),
        (,) "実績" (printf "%s/%s" (show $ M.size $ g^.achieves) (show $ length $ achievementList))]

    m as = fmap (f as) achievementList
    f as (s,_) = case M.lookup s as of
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
  dmul <- use dependCoeff
  lmul <- use lpsCoeff

  case f' of
    True -> do
      f <- use hasFocus
      when (f == False) $ do
        let bonus = (diff/1000/50) * dmul
        depend += bonus
        putAlert "愛ちゃん" $ "おかえりなさい！<br>(依存度ボーナス +" ++ humanize bonus ++ ")"

      depend += (diff/1000/1000) * dmul
      lps += (diff/1000/100) * lmul

      lastFocus .= now

    False -> do
      let p = 0.01 * lmul
      depend %= (\x -> (x-p) `max` 0)
      when ((ai^.depend) > 0) $
        lps += p
  hasFocus .= f'
  achievesCheck
  maxCheck
  itemAffordableCheck

  where
    achievesCheck = do
      forM_ achievementList $ \(s,m) -> do
        as <- use achieves
        when (M.notMember s as || as M.! s == Nothing) $ m
    maxCheck = do
      ml <- use maxLoves
      lv <- use loves
      when (lv > ml) $ maxLoves .= lv
    itemAffordableCheck = do
      im <- use items
      forM_ shopItemList $ \(i,(c,_,_)) -> do
        let eid = "item-" ++ i
        let num = maybe 0 id (M.lookup i im)
        let cond = if i `notElem` normalItemNames then num == 0 else True
        lvs <- use loves
        when (cond && fromIntegral (c num) <= lvs) $
          withElem (eid ++ "-btn") $ \ebtn -> do
            removeAttr ebtn "disabled"

loadItem :: (Serialize a) => String -> a -> IO (IORef a)
loadItem s a = do
  g <- getItem saveName
  newIORef $ case g of
    Left _ -> a
    Right r -> r

save :: StateT Aichan IO ()
save = do
  lift . setItem saveName =<< get
  displayShop
  displayAchievements

main = do
  ref <- loadItem saveName =<< def
  let (a,b) = partition (\(x,_) -> x `elem` normalItemNames) shopItemList
  withElem "list-group" $ \e -> do
    forM_ a $ \(i,(_,_,(icon,tip,box))) -> do
      P.build (itemLi ref i icon tip box) e
  withElem "list-sp-group" $ \e -> do
    forM_ b $ \(i,(_,_,(icon,tip,box))) -> do
      P.build (itemLi ref i icon tip box) e

  btnEvents ref $ fmap fst shopItemList
  withElem "data-erase" $ P.build $ P.addEvent P.this OnClick $ \_ _ -> do
    refStateT ref $ do
      put =<< liftIO def
      save
      withElem "monitor" $ \e -> setStyle e "display" "none"
      withElem "itemshop" $ \e -> setStyle e "display" "none"
      withElem "item-monitor" $ \e -> setStyle e "display" "block"
      withElem "item-itemshop" $ \e -> setStyle e "display" "block"

  let recover = ["monitor","itemshop"]
  forM_ (M.assocs $ M.filterWithKey (\k _ -> k `elem` recover) shopItems) $ \(i,t) -> do
    let (_,m,_) = t
    refStateT ref $ do
      im <- use items
      when (M.member i im && im M.! i > 0) $ m $ im M.! i

  refStateT ref $ do
    displayShop

  q <- loopStateT (floor $ 1000/fps) ref $ do
    im <- use items
    when (M.member "monitor" im) update
    display
  print q
  
  q1 <- loopStateT (floor $ 1000/titleFps) ref $ do
    lift . setTitle =<< humanize <$> use loves
  print q1

  q2 <- loopStateT (saveInterval * 1000) ref $ do
    save
    putAlert "auto" "セーブしました"
  print q2
  
itemLi :: IORef Aichan -> String -> String -> String -> String -> P.Perch
itemLi ref i icon tips box = do
  let p = "item-" ++ i
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

btnEvents :: IORef Aichan -> [String] -> IO ()
btnEvents _ [] = return ()
btnEvents ref (i:is) = do
  ei <- elemById $ "item-" ++ i ++ "-btn"
  case ei of
    Just ebtn -> do
      onEvent ebtn OnClick $ \_ _ -> do
        when (i `elem` ["reset","resetAll"]) $ do
          k <- eval $ toJSString "window.confirm(\"この操作は取り消せません。よろしいですか？\");"
          print k
        refStateT ref $ do
          items %= M.insertWith (+) i 1
          let (c,m,(_,_,name)) = shopItems M.! i
          num <- (M.! i) <$> use items
          loves -= fromIntegral (c $ num-1)
          m num
          putLog "game" $ "「" ++ name ++ "」を購入しました"
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
