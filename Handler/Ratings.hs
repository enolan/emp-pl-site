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
        Just prgmScore' ->
          void $ rollbackIfBadPoints uid $ do
            programRes <- insertBy Program {programName = prgmName}
            let programKey = either entityKey id programRes
            void $ upsert
              Rating
              {ratingUser = uid, ratingProgram = programKey,
               ratingScore = prgmScore'}
              [RatingScore =. prgmScore']
        Nothing -> invalidArgs []
    _ -> invalidArgs []

deleteRatingR :: Handler ()
deleteRatingR = do
  uid <- requireAuthId
  mbPrgmName <- lookupPostParam "prgmName"
  case mbPrgmName of
    Just prgmName ->
      void $ rollbackIfBadPoints uid $ do
        prgmId <- getBy (UniqueName prgmName)
        case prgmId of
          Just prgmEnt -> do
            deleteBy (UniqueUserProgramPair uid (entityKey prgmEnt))
          Nothing -> invalidArgs []
    Nothing -> invalidArgs []

getPoints :: Handler (Int,Int)
getPoints = do
  uid <- requireAuthId
  runDB $ getPointsDB uid

getPointsDB :: MonadIO m => Key User -> ReaderT SqlBackend m (Int, Int)
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

-- Run a DB action, if that action put the user into negative points, abort the
-- transaction and send 400 invalid arguments. Otherwise, return the action's
-- result.
rollbackIfBadPoints :: Key User -> YesodDB App a -> Handler (Maybe a)
rollbackIfBadPoints uid act = do
  mbRes <- runDB $ do
    res <- act
    (ptsSpent, totalPoints) <- getPointsDB uid
    if ptsSpent > totalPoints
      then transactionUndo >> return Nothing
      else return $ Just res
  case mbRes of
    Nothing -> do
      invalidArgs ["Change would use more points than you have"]
    Just res -> return $ Just res

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
