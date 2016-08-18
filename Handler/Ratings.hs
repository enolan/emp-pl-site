module Handler.Ratings where

import Import

import Database.Esqueleto hiding ((==.))
import qualified Database.Esqueleto as E

newtype Ratings = Ratings [Rating]
  deriving Generic

instance ToJSON Ratings
instance ToContent Ratings where
  toContent = toContent . toJSON
instance ToTypedContent Ratings where
  toTypedContent = toTypedContent . toJSON

postRatingR :: Handler ()
postRatingR = return ()

getRatingsRandom :: HowMany -> Handler [Rating]
getRatingsRandom howmany = do
  uid <- requireAuthId
  map entityVal <$> runDB (select $ from $
      \rating -> do
        where_ (val uid E.==. rating ^. RatingUser)
        orderBy [rand]
        case howmany of
          All -> return ()
          Some i -> limit (fromIntegral i)
        return rating)

getRatingsR :: RatingSort -> HowMany -> Handler Ratings
getRatingsR RandomSort howmany = Ratings <$> getRatingsRandom howmany
getRatingsR rsort      howmany = do
  uid <- requireAuthId
  Ratings . map entityVal <$> runDB (
    selectList [RatingUser ==. uid] (
      (case rsort of
          HighestScore -> Desc RatingScore
          LowestScore  -> Asc  RatingScore
          RandomSort   -> error "impossible: RandomSort in getRatingsR") :
       case howmany of
          All -> []
          Some i -> [LimitTo i]))
