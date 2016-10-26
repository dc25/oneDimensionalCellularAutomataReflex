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
                        , ("cy", pack $ show y)
                        , ("cx", pack $ show x)
                        ]
    elDynAttrNS' svgns "circle" (fmap attrs dynXY) $ return ()
    return ()

updateModel :: a -> (Int,Maybe Model) -> (Int,Maybe Model)
updateModel _ (index, Just (x,y)) = (index+1,Just (x+100, y+100))

initModel :: Model
initModel = (50,50)

main :: IO ()
main = mainWidget $ do
    tickEvent <- tickLossy  updateFrequency =<< liftIO getCurrentTime
    let attrs = constDyn $ 
                    DM.fromList 
                        [ ("width" , pack $ show width)
                        , ("height", pack $ show height)
                        , ("style" , "border:solid; margin:8em")
                        ]

        modelToMap = fromList.(\x->x:[])

        initial = modelToMap (0,initModel)

    progress <- foldDyn updateModel (0,Just initModel) tickEvent

    let progressEvents = updated $ fmap modelToMap progress
    elDynAttrNS' svgns "svg" attrs $ listWithKeyShallowDiff initial progressEvents showEvent
    return ()
