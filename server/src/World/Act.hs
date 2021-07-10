module World.Act where

import Control.Monad.IO.Class (liftIO)

import Client (Client(..))
import Config (App)
import World.World (WorldRequest)
import qualified World.WorldProgress (WorldProgress)

import qualified World.Decision as World
import qualified World.Encounter as World
import qualified World.World as World
import qualified World.WorldProgress as World



begin :: TVar Server.State -> Client -> Maybe WorldProgress -> App ()
begin state client mProgress = do
  Client.send "acceptWorld:" client
  let username = Client.queryUsername client
  progress <- World.refreshProgress <$> fromMaybe (World.loadProgress username) (return <$> mProgress)
  World.updateProgress username progress
  world <- World.getWorld state progress
  Client.send (cs $ "world:" <> encode world) client
  msg <- Client.receive client
  let req = World.parseRequest msg
  World.act req world

act : Maybe WorldRequest -> World -> App ()
act (Just req) world =
  case (req, World.world_decision world) of
    (World.JoinEncounter encounterId, Nothing) ->
      case World.encounterFromGuid world encounterId of
        Just encounter -> do
          liftIO $ Log.info $ printf "<%s>: Joining world encounter %s" (show $ Client.name client) (encounterId)
          Client.send ("joinEncounter:" <> encounterId) client
          gen <- liftIO getGen
          let scenario = World.makeScenario progress encounter
          let roomName = encounterId
          let cpuName = World.encounter_name encounter

          let preProgress = World.preEncounterProgress encounter progress
          World.updateProgress username preProgress

          liftIO $ Log.info $ printf "<%s>: Checking for world pvp encounter" (show $ Client.name client)
          mPvpRoom <- liftIO . atomically $ Server.peekqueue state

          case mPvpRoom of
            Just (_, roomVar) -> do
              _ <- liftIO . atomically $ Server.modScenario (World.pvpScenario progress) roomVar
              beginPlay state client roomVar
              beginWorld state client (Just postProgress)
            Nothing -> do
              roomVar <- liftIO . atomically $ Server.getOrCreateRoom roomName WaitCustom gen scenario state
              didWin <- beginComputer cpuName state client roomVar
              if didWin then
                (do
                  let postProgress = World.postEncounterProgress encounter scenario progress
                  liftIO $ Log.info $ printf "<%s>: Win! New world progress %s" (show $ Client.name client) (show postProgress)
                  beginWorld state client (Just postProgress)
                )
                else
                  (do
                    let newProgress = World.initialProgress (World.nextGen progress)
                    liftIO $ Log.info $ printf "<%s>: Loss! New world progress %s" (show $ Client.name client) (show newProgress)
                    beginWorld state client (Just newProgress)
                  )
        Nothing -> do
          liftIO $ Log.error $ printf "<%s>: No such encounter" (show $ Client.name client)
          Client.send "error:no such encounter" client
    (World.EncounterDecision choiceIndex, Just decision) -> do
      liftIO $ Log.info $ printf "<%s>: Making decision %s" (show $ Client.name client) (show choiceIndex)
      case atMay (World.decision_choices decision) choiceIndex of
        Just choice -> do
          let eff = World.decisionchoice_eff choice
          -- Note that we don't get the next gen here
          let newProgress = eff progress { World.worldprogress_decisionId = Nothing, World.worldprogress_roomId = Nothing }
          beginWorld state client (Just newProgress)
        _ -> do
          liftIO $ Log.error $ printf "<%s>: Invalid choice %s" (show $ Client.name client) (show choiceIndex)
          Client.send "error:invalid choice" client
          beginWorld state client (Just progress)
    _ -> do
      liftIO $ Log.error $ printf "<%s>: Illegal world request for state '%s'" (show $ Client.name client) msg
      Client.send "error:illegal world request for state" client
      beginWorld state client (Just progress)
act Nothing world = do
  liftIO $ Log.error $ printf "<%s>: Unknown world request '%s'" (show $ Client.name client) msg
  Client.send "error:unknown world request" client
  beginWorld state client (Just progress)
