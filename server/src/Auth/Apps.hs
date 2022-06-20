module Auth.Apps where

import Auth.Schema as Schema
import Config (App, runBeam, runBeamIntegrity, runRedis)
import Control.Exception (throw)
import Control.Monad.IO.Class (liftIO)
import Crypto.BCrypt (validatePassword)
import Data.Aeson (encode)
import Data.ByteString (ByteString, length)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Beam (all_, default_, delete, filter_, insert, insertExpressions, insertValues, runDelete, runInsert, runSelectReturningOne, select, val_, (==.))
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.PostgreSQL.Simple.Errors (ConstraintViolation (..))
import qualified Database.Redis as R
import qualified Network.WebSockets as WS
import Schema (GalgagameDb (..), galgagameDb)
import Stats.Progress (Progress (..), initialProgress)
import Stats.Schema as Schema
import qualified Stats.Stats as Stats
import System.Log.Logger (errorM)
import Text.Printf (printf)
import Web.Cookie (parseCookiesText)
import Prelude hiding (length)

type Token = Text

type Username = Text

type Seconds = Integer

type Cookies = Map Text Text

saveSession :: ByteString -> Token -> App ()
saveSession username token = do
  let tokenBytestring = cs token :: ByteString
  runRedis $ do
    _ <- R.set tokenBytestring username
    return ()

checkAuth :: Maybe Token -> App (Maybe Username)
checkAuth Nothing = return Nothing
checkAuth (Just token) = do
  result <- runRedis $ R.get $ T.encodeUtf8 token
  case result of
    Right username ->
      return $ T.decodeUtf8 <$> username
    Left err -> do
      liftIO $ errorM "auth" $ printf "Database connection error %s" (show err)
      return $ Nothing

deleteToken :: Token -> App ()
deleteToken token = do
  _ <- runRedis $ R.del [T.encodeUtf8 token]
  return ()

saveUser :: ByteString -> ByteString -> ByteString -> Bool -> Maybe Text -> App Bool
saveUser email username hashedPassword contactable mCid = do
  progress <- fromMaybe initialProgress <$> Stats.loadByCid mCid
  result <- runBeamIntegrity $ do
    [userResult] <-
      runInsertReturningList $
        insert (users galgagameDb) $
          insertExpressions
            [ Schema.User
                default_
                (val_ (cs email))
                (val_ (cs username))
                (val_ (cs hashedPassword))
                (val_ contactable)
                (val_ False)
            ]
    let xp = progress_xp progress
    let stat = Schema.Stats (Schema.UserId $ userId userResult) xp (Just . cs $ encode progress)
    _ <- runInsert $ insert (stats galgagameDb) $ insertValues [stat]
    case mCid of
      Just cid ->
        runDelete $ delete (statsguest galgagameDb) (\row -> statsguestCid row ==. val_ cid)
      Nothing ->
        return ()
    return userResult
  case result of
    Right _ ->
      return True
    Left (UniqueViolation "users_username_unique") ->
      return False
    Left err ->
      throw err

checkPassword :: ByteString -> ByteString -> App Bool
checkPassword username password = do
  user <-
    runBeam $
      runSelectReturningOne $
        select $
          filter_ (\row -> Schema.userUsername row ==. val_ (cs username)) $
            all_ $ users galgagameDb
  return $
    case Schema.userPasshash <$> user of
      Just hashedPassword ->
        validatePassword (cs hashedPassword) password
      Nothing ->
        False

sessionCookieName :: Text
sessionCookieName = "login"

getCookies :: WS.PendingConnection -> Cookies
getCookies pending = Map.fromList cookiesList
  where
    cookieHeaders :: WS.Headers
    cookieHeaders = WS.requestHeaders . WS.pendingRequest $ pending
    cookieString :: Maybe ByteString
    cookieString = snd <$> find ((==) "Cookie" . fst) cookieHeaders
    cookiesList :: [(Text, Text)]
    cookiesList = maybe [] parseCookiesText cookieString

legalName :: ByteString -> Either Text ()
legalName name
  | length name < 3 = Left "Username too short"
  | length name > 12 = Left "Username too long"
  | invalidNameChars name = Left "Username contains invalid characters"
  | otherwise = Right ()

invalidNameChars :: ByteString -> Bool
invalidNameChars nameRaw =
  let name = cs nameRaw :: Text
      sanitizedName = T.filter validChars name :: Text
      validChars :: Char -> Bool
      validChars char =
        elem char $
          ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_', '-', '.']
   in T.length name /= T.length sanitizedName

legalPassword :: ByteString -> Either Text ()
legalPassword p
  | length p < 8 = Left "Password too short"
  | otherwise = Right ()

cidCookieName :: Text
cidCookieName = "cid"
