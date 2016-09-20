module Handler.Ratings where

import Import

import Database.Esqueleto hiding ((=.),(==.))
import qualified Database.Esqueleto as E

data Ratings = Ratings
  {ratingsRatings :: [(Text, Int)],
   ratingsPtsSpent :: Int,
   ratingsTotalBudget :: Int}
  deriving Generic

instance ToJSON Ratings
instance ToContent Ratings where
  toContent = toContent . toJSON
instance ToTypedContent Ratings where
  toTypedContent = toTypedContent . toJSON

postRatingR :: Handler ()
postRatingR = do
  uid <- requireAuthId
  mbAction <- lookupPostParam "action"
  mbPrgmName <- lookupPostParam "prgmName"
  mbPrgmScore <- lookupPostParam "prgmScore"
  case (mbAction, mbPrgmName, mbPrgmScore) of
    (Just _action, Just prgmName, Just prgmScore) ->
      case readMay prgmScore of
        Just prgmScore' -> do
          programRes <- runDB $ insertBy
            Program {programName = prgmName}
          let programKey = either entityKey id programRes
          _ratingRes <- runDB $ upsert
            Rating
             {ratingUser = uid, ratingProgram = programKey,
              ratingScore = prgmScore'}
            [RatingScore =. prgmScore']
          pure ()
        Nothing -> invalidArgs []
    _ -> invalidArgs []

deleteRatingR :: Handler ()
deleteRatingR = do
  uid <- requireAuthId
  mbPrgmName <- lookupPostParam "prgmName"
  case mbPrgmName of
    Just prgmName -> runDB $ do
      prgmId <- getBy (UniqueName prgmName)
      case prgmId of
        Just prgmEnt -> deleteBy (UniqueUserProgramPair uid (entityKey prgmEnt))
        Nothing -> invalidArgs []
    Nothing -> invalidArgs []

getPoints :: Handler (Int,Int)
getPoints = do
  uid <- requireAuthId
  runDB $ getPointsDB uid

getPointsDB ::
  MonadIO m => Key User -> ReaderT SqlBackend m (Int, Int)
getPointsDB uid = do
  res <- select $
    from $ \rating -> do
      where_ $ val uid E.==. (rating ^. RatingUser)
      let score = rating ^. RatingScore
          ratingCount = E.count score :: SqlExpr (E.Value Int)
      return (sum_ $ score *. score, ratingCount *. val 25)
  -- Postgres' SUM on bigints returns numeric which is interpreted as Rational
  let ratToInt :: Rational -> Int
      ratToInt x = floor (fromRational x :: Float)
  case res of
    [(Value (Just ptsSpent), Value totalPoints)] ->
      return (ratToInt ptsSpent, totalPoints)
    _ -> return (0, 0)

getRatings :: Handler Ratings
getRatings = do
  uid <- requireAuthId
  ratingsRatings <- map (\(Value a, Value b) -> (a, b)) <$> runDB (
    select $
      from $ \(rating `InnerJoin` program) -> do
        E.on (rating ^. RatingProgram E.==. program ^. ProgramId)
        where_ (rating ^. RatingUser E.==. val uid)
        return (program ^. ProgramName, rating ^. RatingScore))
  (ratingsPtsSpent, ratingsTotalBudget) <- getPoints
  return Ratings{..}
