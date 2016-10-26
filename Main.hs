{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom
import Data.Map (Map, fromList)
import Data.Text (Text, pack)
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Control.Monad.Trans (liftIO)


height = 600
width = 600

rowCount = 100
colCount = 100

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

updateFrequency :: NominalDiffTime
updateFrequency = 0.1

type Model = (Int,Int)

showEvent :: (MonadWidget t m) => Int -> Model -> Event t Model -> m ()
showEvent index v@(x,y) _ = do
    let dynXY = constDyn v
        attrs (x,y) = fromList 
                        [ ("r" , "10.0")
                        , ("fill", "purple")
                        , ("cy", pack $ show y)
                        , ("cx", pack $ show x)
                        ]
    elDynAttrNS' svgns "circle" (fmap attrs dynXY) $ return ()
    return ()

step :: a -> (Int,Maybe Model) -> (Int,Maybe Model)
step _ (index, Just (x,y)) = (index+1,Just (x+5, y+5))

initModel :: Model
initModel = (50,50)

pairToMap :: Ord k => (k, v) -> Map k v
pairToMap = fromList.(\x->x:[])

main :: IO ()
main = mainWidget $ do
    let attrs = constDyn $ 
                    fromList 
                        [ ("width" , pack $ show width)
                        , ("height", pack $ show height)
                        , ("style" , "border:solid; margin:8em")
                        ]

        initial = pairToMap (0,initModel)

    progress <- foldDyn step (0,Just initModel) 
                    =<< tickLossy  updateFrequency 
                    =<< liftIO getCurrentTime

    let progressEvents = updated $ fmap pairToMap progress
    elDynAttrNS' svgns "svg" attrs $ listWithKeyShallowDiff initial progressEvents showEvent
    return ()
