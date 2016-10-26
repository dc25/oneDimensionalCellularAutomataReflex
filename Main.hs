{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom
import Data.Map as DM (fromList)
import Data.Text as DT (Text, pack)
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Control.Monad.Trans (liftIO)


height = 600
width = 600

rowCount = 100
colCount = 100

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

updateFrequency :: NominalDiffTime
updateFrequency = 0.5

showEvent :: (MonadWidget t m) => Int -> (Int,Int) -> Event t (Int,Int) -> m ()
showEvent index (x,y) e = do
    dynXY <- holdDyn (x,y) e
    let attrs (x,y) = DM.fromList 
                        [ ("r" , "10.0")
                        , ("fill", "purple")
                        , ("cy", pack $ show x)
                        , ("cx", pack $ show y)
                        ]
    elDynAttrNS' svgns "circle" (fmap attrs dynXY) $ return ()
    return ()

main :: IO ()
main = mainWidget $ do
    tickEvent <- tickLossy  updateFrequency =<< liftIO getCurrentTime
    let attrs = constDyn $ 
                    DM.fromList 
                        [ ("width" , pack $ show width)
                        , ("height", pack $ show height)
                        , ("style" , "border:solid; margin:8em")
                        ]
        initialMap = fromList  [(0,(100,100))]
        mapEvent = fmap (const $ fromList [(1,Just (200,200))]) tickEvent
    elDynAttrNS' svgns "svg" attrs $ listWithKeyShallowDiff initialMap mapEvent showEvent
    return ()
