{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Data.Map (Map, fromList)
import Data.Text (Text, pack)
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Control.Monad.Trans (liftIO)

rowCount = 200
colCount = 400

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

type Model = [Bool]

showCell :: (MonadWidget t m) => Int -> Dynamic t Int -> m ()
showCell level column = do
    let attrs column = fromList 
                           [ ("r" , "0.3")
                           , ("fill", "purple")
                           , ("cy", pack $ show level)
                           , ("cx", pack $ show column)
                           ]
    elDynAttrNS' svgns "circle" (fmap attrs column) $ return ()
    return ()

showRow :: (MonadWidget t m) => Int -> Model -> m ()
showRow level v = do
    let living = fmap (\(index, live) -> index) 
                 $ filter (\(index, live) -> live) 
                 $ zip [0..] v

    simpleList (constDyn living) (showCell level)
    return ()

step :: a -> (Int,Maybe Model) -> (Int,Maybe Model)
step _ v@(level, Just model) = 
    if level == rowCount then v
    else let gen d0 d1 d2 = 
              case (d0,d1,d2) of
                (False,  False,   True) -> True
                (False,   True,  False) -> True
                (False,   True,   True) -> True
                ( True,  False,  False) -> True
                _                       -> False
             left = (tail model ++ [False])
             right = (False : model)
             nextGen = zipWith3 gen left model right
         in (level+1, Just nextGen)
    
init :: Model
init = 
    let halfCount = colCount `div` 2 
        halfSpan = take halfCount $ repeat False
    in tail  (halfSpan ++ True : halfSpan)

main :: IO ()
main = mainWidget $ do
    let pairToMap = fromList.(\x->x:[])
        attrs = constDyn $ 
                    fromList 
                        [ ("width" , "1000")
                        , ("height", "500")
                        , ("style" , "border:solid; margin:8em")
                        , ("viewBox" , pack $ unwords
                                                [ show (0 :: Int)
                                                , show (0 :: Int)
                                                , show colCount
                                                , show rowCount
                                                ] )
                        ]

        initial = pairToMap (0,Main.init)

    progress <- foldDyn step (0,Just Main.init) 
                =<< tickLossy  0.1
                =<< liftIO getCurrentTime

    let progressEvents = updated $ fmap pairToMap progress
    elDynAttrNS' svgns "svg" attrs $ listHoldWithKey initial progressEvents showRow
    return ()
