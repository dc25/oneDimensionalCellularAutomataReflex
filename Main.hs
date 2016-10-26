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

type Model = (Int,Int)

showEvent :: (MonadWidget t m) => Int -> Model -> Event t Model -> m ()
showEvent index v@(x,y) _ = do
    let dynXY = constDyn v
        attrs (x,y) = DM.fromList 
                        [ ("r" , "10.0")
                        , ("fill", "purple")
                        , ("cy", pack $ show x)
                        , ("cx", pack $ show index)
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

        initial = (0,(50,50))

    progress <- foldDyn (\_ (index,Just (cx, cy)) -> (index+1,Just (cx +100, cy+100))) 
                        (0,Just (100,100))
                        tickEvent
    let progressEvents = updated $ fmap (fromList.(\x->x:[])) progress
    elDynAttrNS' svgns "svg" attrs $ listWithKeyShallowDiff (fromList [initial]) progressEvents showEvent
    return ()
