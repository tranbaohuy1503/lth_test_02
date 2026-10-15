-- ============================================================================
-- ĐỒ ÁN CUỐI KỲ: MULTIPLAYER TACTICAL 3D HERO TANK BATTLE ARENA (NHÓM 5 THÀNH VIÊN)
-- File: src/Client/Main.hs
-- Phụ trách: THÀNH VIÊN 5 (Giao Diện Đồ Họa 3D Gloss & Điểm Khởi Chạy Game)
-- ============================================================================
-- [Buoi_02_IO_va_Monad_IO] & [LTH_Chuong_7_Evaluation_Functor_Applicative_Monad]
-- 1. gloss playIO Game Loop (Vòng lặp trò chơi thuần túy với OpenGL):
--    - renderApp       :: ClientWorld -> IO Picture
--    - handleAppEvent  :: Event -> ClientWorld -> IO ClientWorld
--    - stepAppWorld    :: Float -> ClientWorld -> IO ClientWorld
-- 2. Đa phân cảnh (Multi-Screen State Machine):
--    - LoginScreen: Giao diện đăng nhập, cấu hình phi công & Showroom 3D.
--    - ConnectingScreen: Màn hình kết nối máy chủ qua mạng TCP.
--    - PlayingScreen: Đấu trường không gian 3D 32x22, HUD, Minimap & Skill Dock.
-- 3. Nội suy mượt mà (Position & Heading Lerp):
--    - Xử lý lệch pha 30 FPS Server -> 60 FPS Client, di chuyển và xoay nòng chuẩn xác.
-- 4. Real-time Audio Dispatcher (Client.Audio):
--    - Nhạc nền BGM lặp vô hạn qua Windows MCI.
--    - Âm thanh SFX súng pháo chân thực và Takedown qua WinMM PlaySound API.
-- ============================================================================

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Types
import GameLogic (updateGameState, stepGameTick)
import Protocol (encodeCommand, decodeGameState, decodeClientId)
import Client.UI
  ( AppScreen(..)
  , InputField(..)
  , Particle(..)
  , Shockwave(..)
  , nextClass
  , prevClass
  , mkCallsign
  , mockGameState
  , drawLoginScreen
  , draw3DPlayingScene
  , toScreenPos
  , targetAngle
  )
import Client.Audio
  ( SoundEvent(..)
  , initAudioSystem
  , playSound
  , startBgmLoop
  , setBgmMute
  )
import Server.Network (receiveLinesLoop)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Network.Socket
  ( Socket, AddrInfo(..), SocketType(Stream), defaultHints
  , getAddrInfo, socket, connect, close
  )
import Network.Socket.ByteString (sendAll)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, modifyTVar')
import Control.Exception (SomeException, catch)
import System.Environment (getArgs)
import System.IO (hSetEncoding, hSetBuffering, stdout, stderr, utf8, BufferMode(LineBuffering))
import qualified Data.Map.Strict as Map
import Data.Char (isDigit, isAlphaNum)
import Data.Fixed (mod')

-- | Cấu trúc dữ liệu thế giới Client hợp nhất mọi phân cảnh
data ClientWorld = ClientWorld
  { worldScreen       :: !AppScreen                    -- ^ Trạng thái phân cảnh hiện tại
  , worldCallsign     :: !String                       -- ^ Tên định danh phi công
  , worldServerHost   :: !String                       -- ^ Địa chỉ IP máy chủ kết nối
  , worldServerPort   :: !String                       -- ^ Cổng dịch vụ máy chủ
  , worldActiveField  :: !InputField                   -- ^ Trường nhập liệu đang chọn
  , worldClass        :: !TankClass                    -- ^ Lớp tướng chiến binh đang trang bị
  , worldLoginError   :: !(Maybe String)               -- ^ Thông báo lỗi đăng nhập (nếu có)
  , worldAnimTime     :: !Float                        -- ^ Biến đếm thời gian hiệu ứng
  , worldMyId         :: !(TVar Int)                   -- ^ ID do máy chủ cấp phát (TVar STM)
  , worldStateVar     :: !(TVar GameState)             -- ^ Trạng thái thế giới game (TVar STM)
  , worldPrevState    :: !GameState                    -- ^ Trạng thái nhịp trước để bắt sự kiện nổ
  , worldVisualTanks  :: !(Map.Map Int (Float, Float)) -- ^ Tọa độ nội suy mượt mà (Position Lerp)
  , worldVisualAngles :: !(Map.Map Int Float)          -- ^ Góc xoay nòng mượt mà (Heading Lerp)
  , worldParticles    :: ![Particle]                   -- ^ Hệ thống hạt tàn lửa 3D
  , worldShockwaves   :: ![Shockwave]                  -- ^ Danh sách sóng xung kích lan tỏa
  , worldSocket       :: !(Maybe Socket)               -- ^ Socket TCP nếu đang Online
  , worldIsOnline     :: !Bool                         -- ^ Cờ đánh dấu chế độ mạng
  , worldIsMuted      :: !Bool                         -- ^ Cờ bật/tắt âm thanh
  }

-- ============================================================================
-- ĐIỂM KHỞI CHẠY CHÍNH CỦA ỨNG DỤNG (ENTRY POINT)
-- ============================================================================

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  args <- getArgs
  let isOfflineArg = "--offline" `elem` args

  putStrLn "============================================================"
  putStrLn "   TACTICAL 3D HERO TANK BATTLE ARENA // FINAL PROJECT     "
  putStrLn "   GLOSS 2.5D EXTENSION // 5 HEROES // 32x22 MULTIPLAYER   "
  putStrLn "============================================================"

  -- Khởi tạo hệ sinh thái âm thanh procedural WAV
  initAudioSystem

  -- Bật nhạc nền Cyber Synth BGM loop ngay khi khởi động
  startBgmLoop False

  idVar <- newTVarIO 1
  stVar <- newTVarIO mockGameState

  let initialWorld = ClientWorld
        { worldScreen       = if isOfflineArg then PlayingScreen else LoginScreen
        , worldCallsign     = "COMMANDER"
        , worldServerHost   = "127.0.0.1"
        , worldServerPort   = "9123"
        , worldActiveField  = FieldCallsign
        , worldClass        = VanguardCyan
        , worldLoginError   = Nothing
        , worldAnimTime     = 0.0
        , worldMyId         = idVar
        , worldStateVar     = stVar
        , worldPrevState    = mockGameState
        , worldVisualTanks  = Map.empty
        , worldVisualAngles = Map.empty
        , worldParticles    = []
        , worldShockwaves   = []
        , worldSocket       = Nothing
        , worldIsOnline     = False
        , worldIsMuted      = False
        }

  -- Cửa sổ kích thước chuẩn 1280x720 (vừa vặn tuyệt đối trên mọi độ phân giải)
  let windowDisplay = InWindow "Multiplayer Tactical 3D Hero Tank Battle Arena"
                               (1280, 720)
                               (80, 40)
      bgColor       = black
      targetFPS     = 60

  playIO
    windowDisplay
    bgColor
    targetFPS
    initialWorld
    renderApp
    handleAppEvent
    stepAppWorld

-- ============================================================================
-- 1. KẾT XUẤT ĐỒ HỌA THEO PHÂN CẢNH (SCENE DISPATCHER)
-- ============================================================================

renderApp :: ClientWorld -> IO Picture
renderApp world = case worldScreen world of
  LoginScreen ->
    return $ drawLoginScreen
      (worldAnimTime world)
      (worldCallsign world)
      (worldServerHost world)
      (worldServerPort world)
      (worldActiveField world)
      (worldClass world)
      (worldLoginError world)

  ConnectingScreen msg ->
    return $ pictures
      [ color (makeColorI 7 10 16 255) (rectangleSolid 1400.0 800.0)
      , translate 0.0 40.0 $ rotate (worldAnimTime world * 120.0) $
          color (makeColorI 0 240 255 200) (thickCircle 35.0 4.0)
      , translate 0.0 (-40.0) $ pictures
          [ translate (-120.0) 0.0 $ scale 0.13 0.13 $
              color (makeColorI 0 240 255 255) (text "DANG KET NOI...")
          , translate (-180.0) (-20.0) $ scale 0.085 0.085 $
              color white (text msg)
          ]
      ]

  PlayingScreen -> do
    !myId <- readTVarIO (worldMyId world)
    !st   <- readTVarIO (worldStateVar world)
    return $ draw3DPlayingScene
      (worldAnimTime world)
      myId
      (worldCallsign world)
      st
      (worldVisualTanks world)
      (worldVisualAngles world)
      (worldParticles world)
      (worldShockwaves world)
      (worldIsOnline world)
      (worldIsMuted world)

-- ============================================================================
-- 2. XỬ LÝ SỰ KIỆN PHÍM VÀ CHUỘT (INTERACTION EVENT HANDLING)
-- ============================================================================

handleAppEvent :: Event -> ClientWorld -> IO ClientWorld
handleAppEvent event world = case worldScreen world of
  LoginScreen        -> handleLoginEvent event world
  ConnectingScreen _ -> handleConnectingEvent event world
  PlayingScreen      -> handlePlayingEvent event world

handleLoginEvent :: Event -> ClientWorld -> IO ClientWorld
handleLoginEvent event world = case event of
  EventKey (SpecialKey KeyEnter) Down _ _ ->
    tryConnectOnline world

  EventKey (SpecialKey KeyF1) Down _ _ ->
    startOfflineArena world

  EventKey (SpecialKey KeyTab) Down _ _ ->
    let nextField = case worldActiveField world of
                      FieldCallsign   -> FieldServerHost
                      FieldServerHost -> FieldServerPort
                      FieldServerPort -> FieldCallsign
    in return world { worldActiveField = nextField, worldLoginError = Nothing }

  EventKey (SpecialKey KeyUp) Down _ _ ->
    let prevField = case worldActiveField world of
                      FieldCallsign   -> FieldServerPort
                      FieldServerHost -> FieldCallsign
                      FieldServerPort -> FieldServerHost
    in return world { worldActiveField = prevField, worldLoginError = Nothing }

  EventKey (SpecialKey KeyDown) Down _ _ ->
    let nextField = case worldActiveField world of
                      FieldCallsign   -> FieldServerHost
                      FieldServerHost -> FieldServerPort
                      FieldServerPort -> FieldCallsign
    in return world { worldActiveField = nextField, worldLoginError = Nothing }

  -- Đổi Hero bằng phím mũi tên hoặc A / D
  EventKey (SpecialKey KeyLeft) Down _ _ ->
    return world { worldClass = prevClass (worldClass world) }

  EventKey (SpecialKey KeyRight) Down _ _ ->
    return world { worldClass = nextClass (worldClass world) }

  EventKey (Char 'a') Down _ _ ->
    return world { worldClass = prevClass (worldClass world) }
  EventKey (Char 'A') Down _ _ ->
    return world { worldClass = prevClass (worldClass world) }

  EventKey (Char 'd') Down _ _ ->
    return world { worldClass = nextClass (worldClass world) }
  EventKey (Char 'D') Down _ _ ->
    return world { worldClass = nextClass (worldClass world) }

  -- Phím số 1..5: Chọn trực tiếp lớp tướng
  EventKey (Char '1') Down _ _ -> return world { worldClass = VanguardCyan }
  EventKey (Char '2') Down _ _ -> return world { worldClass = TitanCrimson }
  EventKey (Char '3') Down _ _ -> return world { worldClass = ViperEmerald }
  EventKey (Char '4') Down _ _ -> return world { worldClass = SolarisGold }
  EventKey (Char '5') Down _ _ -> return world { worldClass = PhantomViolet }

  -- Phím xóa
  EventKey (SpecialKey KeyDelete) Down _ _ ->
    return $ deleteCharFromActiveField world
  EventKey (Char '\b') Down _ _ ->
    return $ deleteCharFromActiveField world

  -- Gõ chữ
  EventKey (Char c) Down _ _
    | isAlphaNum c || c `elem` "._-" ->
        return $ appendCharToActiveField c world

  -- Click chuột
  EventKey (MouseButton LeftButton) Down _ (mx, my) ->
    handleClickLoginScreen mx my world

  _ -> return world

handleConnectingEvent :: Event -> ClientWorld -> IO ClientWorld
handleConnectingEvent event world = case event of
  EventKey (SpecialKey KeyEsc) Down _ _ ->
    return world { worldScreen = LoginScreen, worldLoginError = Just "Da huy ket noi!" }
  _ -> return world

handlePlayingEvent :: Event -> ClientWorld -> IO ClientWorld
handlePlayingEvent event world = case event of
  -- Phím [M]: Bật / Tắt âm thanh (Đồng bộ cả SFX lẫn nhạc nền BGM)
  EventKey (Char 'm') Down _ _ -> do
    let newMuted = not (worldIsMuted world)
    setBgmMute newMuted
    return world { worldIsMuted = newMuted }

  EventKey (Char 'M') Down _ _ -> do
    let newMuted = not (worldIsMuted world)
    setBgmMute newMuted
    return world { worldIsMuted = newMuted }

  -- Di chuyển Lên / Bắc (North)
  EventKey (Char 'w') Down _ _           -> sendPlayerCommand world (Move North)
  EventKey (Char 'W') Down _ _           -> sendPlayerCommand world (Move North)
  EventKey (SpecialKey KeyUp) Down _ _    -> sendPlayerCommand world (Move North)

  -- Di chuyển Xuống / Nam (South)
  EventKey (Char 's') Down _ _           -> sendPlayerCommand world (Move South)
  EventKey (Char 'S') Down _ _           -> sendPlayerCommand world (Move South)
  EventKey (SpecialKey KeyDown) Down _ _  -> sendPlayerCommand world (Move South)

  -- Di chuyển Sang Trái / Tây (West)
  EventKey (Char 'a') Down _ _           -> sendPlayerCommand world (Move West)
  EventKey (Char 'A') Down _ _           -> sendPlayerCommand world (Move West)
  EventKey (SpecialKey KeyLeft) Down _ _  -> sendPlayerCommand world (Move West)

  -- Di chuyển Sang Phải / Đông (East)
  EventKey (Char 'd') Down _ _           -> sendPlayerCommand world (Move East)
  EventKey (Char 'D') Down _ _           -> sendPlayerCommand world (Move East)
  EventKey (SpecialKey KeyRight) Down _ _ -> sendPlayerCommand world (Move East)

  -- Khai hỏa súng đại bác Plasma chân thực
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    playSound (worldIsMuted world) SndShoot
    sendPlayerCommand world Shoot

  -- KÍCH HOẠT TUYỆT KỸ TỐI THƯỢNG [E]
  EventKey (Char 'e') Down _ _ -> do
    playSound (worldIsMuted world) SndUltimate
    sendPlayerCommand world CastUltimate

  EventKey (Char 'E') Down _ _ -> do
    playSound (worldIsMuted world) SndUltimate
    sendPlayerCommand world CastUltimate

  -- Đổi nhanh Lớp Tướng bằng phím 1..5 hoặc TAB
  EventKey (Char '1') Down _ _ -> do
    let c = VanguardCyan
    _ <- sendPlayerCommand world (SelectClass c)
    return world { worldClass = c }

  EventKey (Char '2') Down _ _ -> do
    let c = TitanCrimson
    _ <- sendPlayerCommand world (SelectClass c)
    return world { worldClass = c }

  EventKey (Char '3') Down _ _ -> do
    let c = ViperEmerald
    _ <- sendPlayerCommand world (SelectClass c)
    return world { worldClass = c }

  EventKey (Char '4') Down _ _ -> do
    let c = SolarisGold
    _ <- sendPlayerCommand world (SelectClass c)
    return world { worldClass = c }

  EventKey (Char '5') Down _ _ -> do
    let c = PhantomViolet
    _ <- sendPlayerCommand world (SelectClass c)
    return world { worldClass = c }

  EventKey (SpecialKey KeyTab) Down _ _ -> do
    let nextC = nextClass (worldClass world)
    _ <- sendPlayerCommand world (SelectClass nextC)
    return world { worldClass = nextC }

  -- Thoát trận về màn hình sảnh chờ
  EventKey (SpecialKey KeyEsc) Down _ _ -> do
    putStrLn "[Client] Roi tran dau, tro ve Man hinh Dang nhap..."
    case worldSocket world of
      Just sock -> catch (close sock) (\(_ :: SomeException) -> return ())
      Nothing   -> return ()
    return world
      { worldScreen   = LoginScreen
      , worldSocket   = Nothing
      , worldIsOnline = False
      }

  _ -> return world

-- ============================================================================
-- 3. HỖ TRỢ XỬ LÝ CHUỘT VÀ BÀN PHÍM TẠI MÀN HÌNH ĐĂNG NHẬP
-- ============================================================================

appendCharToActiveField :: Char -> ClientWorld -> ClientWorld
appendCharToActiveField c w = case worldActiveField w of
  FieldCallsign ->
    if length (worldCallsign w) < 12
      then w { worldCallsign = worldCallsign w ++ [c], worldLoginError = Nothing }
      else w
  FieldServerHost ->
    if length (worldServerHost w) < 20
      then w { worldServerHost = worldServerHost w ++ [c], worldLoginError = Nothing }
      else w
  FieldServerPort ->
    if length (worldServerPort w) < 5 && isDigit c
      then w { worldServerPort = worldServerPort w ++ [c], worldLoginError = Nothing }
      else w

deleteCharFromActiveField :: ClientWorld -> ClientWorld
deleteCharFromActiveField w = case worldActiveField w of
  FieldCallsign ->
    w { worldCallsign = safeInit (worldCallsign w), worldLoginError = Nothing }
  FieldServerHost ->
    w { worldServerHost = safeInit (worldServerHost w), worldLoginError = Nothing }
  FieldServerPort ->
    w { worldServerPort = safeInit (worldServerPort w), worldLoginError = Nothing }
  where
    safeInit str = if null str then "" else init str

handleClickLoginScreen :: Float -> Float -> ClientWorld -> IO ClientWorld
handleClickLoginScreen mx my world =
  let leftCardX = -265.0
      leftCardY = -20.0
      relX = mx - leftCardX
      relY = my - leftCardY
  in if relX >= -140.0 && relX <= 140.0 && relY >= 88.0 && relY <= 122.0
       then return world { worldActiveField = FieldCallsign, worldLoginError = Nothing }
       else if relX >= -140.0 && relX <= 140.0 && relY >= 13.0 && relY <= 47.0
              then return world { worldActiveField = FieldServerHost, worldLoginError = Nothing }
              else if relX >= -140.0 && relX <= 140.0 && relY >= -62.0 && relY <= -28.0
                     then return world { worldActiveField = FieldServerPort, worldLoginError = Nothing }
                     else if relX >= -140.0 && relX <= 140.0 && relY >= -133.0 && relY <= -97.0
                            then tryConnectOnline world
                            else if relX >= -140.0 && relX <= 140.0 && relY >= -183.0 && relY <= -147.0
                                   then startOfflineArena world
                                   else return world

-- ============================================================================
-- 4. KHỞI CHẠY CHẾ ĐỘ CHƠI (ONLINE & OFFLINE)
-- ============================================================================

startOfflineArena :: ClientWorld -> IO ClientWorld
startOfflineArena world = do
  putStrLn "[Client] Khoi chay che do DIEN TAP NGOAI TUYEN (Practice Arena Offline)..."
  idVar <- newTVarIO 1
  stVar <- newTVarIO mockGameState
  return world
    { worldScreen       = PlayingScreen
    , worldMyId         = idVar
    , worldStateVar     = stVar
    , worldPrevState    = mockGameState
    , worldVisualAngles = Map.empty
    , worldSocket       = Nothing
    , worldIsOnline     = False
    , worldLoginError   = Nothing
    }

tryConnectOnline :: ClientWorld -> IO ClientWorld
tryConnectOnline world = case mkCallsign (worldCallsign world) of
  Left err -> do
    putStrLn $ "[Client] Callsign khong hop le: " ++ err
    return world { worldLoginError = Just err }

  Right validName -> do
    let host = worldServerHost world
        port = worldServerPort world
    putStrLn $ "[Client] Dang ket noi toi May chu " ++ host ++ ":" ++ port ++ "..."

    let hints = defaultHints { addrSocketType = Stream }
    addrsRes <- catch (getAddrInfo (Just hints) (Just host) (Just port))
                      (\(ex :: SomeException) -> do
                        putStrLn $ "[Client] Loi phan giai dia chi: " ++ show ex
                        return []
                      )
    case addrsRes of
      [] -> return world { worldLoginError = Just "Loi IP hoac Port khong ton tai!" }
      (serverAddr:_) -> do
        sock <- socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr)
        connectRes <- catch (do
                              connect sock (addrAddress serverAddr)
                              return True
                            )
                            (\(ex :: SomeException) -> do
                              putStrLn $ "[Client] Ket noi that bai: " ++ show ex
                              return False
                            )
        if not connectRes
          then do
            catch (close sock) (\(_ :: SomeException) -> return ())
            return world { worldLoginError = Just "May chu khong phan hoi (Offline/Sai cong)!" }
          else do
            putStrLn "[Client] Ket noi May chu thanh cong!"
            idVar <- newTVarIO 1
            stVar <- newTVarIO (GameState Map.empty [] Map.empty [] [] 0)

            -- Gửi lệnh chọn lớp tướng đã chọn ngay khi kết nối
            catch (sendAll sock (encodeCommand (SelectClass (worldClass world))))
                  (\(_ :: SomeException) -> return ())

            -- Luồng nền nhận gói tin TCP
            _ <- forkIO $ do
              putStrLn "[Client] Luong nen Green Thread da san sang nhan GameState."
              receiveLinesLoop sock $ \lineBs -> do
                case decodeClientId lineBs of
                  Just assignedId -> do
                    putStrLn $ "[Client] Da nhan ID cap phat tu Server: " ++ show assignedId
                    atomically $ modifyTVar' idVar (\_ -> assignedId)
                  Nothing ->
                    case decodeGameState lineBs of
                      Right !newState ->
                        atomically $ modifyTVar' stVar (\_ -> newState)
                      Left _ -> return ()

            return world
              { worldScreen       = PlayingScreen
              , worldCallsign     = validName
              , worldMyId         = idVar
              , worldStateVar     = stVar
              , worldPrevState    = GameState Map.empty [] Map.empty [] [] 0
              , worldVisualAngles = Map.empty
              , worldSocket       = Just sock
              , worldIsOnline     = True
              , worldLoginError   = Nothing
              }

sendPlayerCommand :: ClientWorld -> Command -> IO ClientWorld
sendPlayerCommand world cmd = do
  myId <- readTVarIO (worldMyId world)
  if worldIsOnline world
    then do
      case worldSocket world of
        Just sock ->
          catch (sendAll sock (encodeCommand cmd))
                (\(ex :: SomeException) -> putStrLn $ "[Client] Loi truyen tin Command: " ++ show ex)
        Nothing -> return ()
    else do
      atomically $ modifyTVar' (worldStateVar world) (\st -> updateGameState myId cmd st)
  return world

-- ============================================================================
-- 5. BƯỚC THỜI GIAN 60 FPS: NỘI SUY (POSITION & HEADING LERP) & HẠT VÀ ÂM THANH
-- ============================================================================

stepAppWorld :: Float -> ClientWorld -> IO ClientWorld
stepAppWorld !dt world = do
  let !newTime = worldAnimTime world + dt

  case worldScreen world of
    LoginScreen -> return world { worldAnimTime = newTime }
    ConnectingScreen _ -> return world { worldAnimTime = newTime }
    PlayingScreen -> do
      if not (worldIsOnline world)
        then atomically $ modifyTVar' (worldStateVar world) stepGameTick
        else return ()

      !curState <- readTVarIO (worldStateVar world)
      let !prevState = worldPrevState world

      -- Bắt các sự kiện va chạm để kích hoạt hiệu ứng hình ảnh và âm thanh 0ms
      let (!newParticles, !newShockwaves, !soundEvents) =
            detectCombatImpacts prevState curState

      -- Phát các âm thanh tương ứng
      mapM_ (playSound (worldIsMuted world)) soundEvents

      let !updatedParticles    = stepParticles dt (newParticles ++ worldParticles world)
          !updatedShockwaves   = stepShockwaves dt (newShockwaves ++ worldShockwaves world)
          !updatedVisualTanks  = interpolateTanks (worldVisualTanks world) (tanks curState)
          !updatedVisualAngles = interpolateAngles (worldVisualAngles world) (tanks curState)

      return world
        { worldAnimTime     = newTime
        , worldPrevState    = curState
        , worldParticles    = updatedParticles
        , worldShockwaves   = updatedShockwaves
        , worldVisualTanks  = updatedVisualTanks
        , worldVisualAngles = updatedVisualAngles
        }

lerpVal :: Float -> Float -> Float -> Float
lerpVal !a !b !factor = a + (b - a) * factor

-- | Nội suy mượt mà vị trí xe tăng (Position Lerp)
interpolateTanks
  :: Map.Map Int (Float, Float)
  -> Map.Map Int Tank
  -> Map.Map Int (Float, Float)
interpolateTanks !curMap !tanksMap =
  foldl' updateSingleTank curMap (Map.toList tanksMap)
  where
    updateSingleTank acc (tId, tank) =
      let (!targetX, !targetY) = toScreenPos (tankPos tank)
          (!curX, !curY)       = Map.findWithDefault (targetX, targetY) tId acc
          !smoothX = lerpVal curX targetX 0.28
          !smoothY = lerpVal curY targetY 0.28
      in Map.insert tId (smoothX, smoothY) acc

-- | Nội suy mượt mà góc xoay nòng xe tăng theo cung ngắn nhất (Heading Lerp)
interpolateAngles
  :: Map.Map Int Float
  -> Map.Map Int Tank
  -> Map.Map Int Float
interpolateAngles !curMap !tanksMap =
  foldl' updateSingleAngle curMap (Map.toList tanksMap)
  where
    updateSingleAngle acc (tId, tank) =
      let !targA = targetAngle (tankDir tank)
          !curA  = Map.findWithDefault targA tId acc
          -- Khoảng cách góc ngắn nhất giữa -180 và +180 độ
          !diff0 = (targA - curA + 180.0) `mod'` 360.0 - 180.0
          !diff  = if diff0 < -180.0 then diff0 + 360.0 else diff0
          !smoothA = curA + diff * 0.32
      in Map.insert tId smoothA acc

-- | Phát hiện va chạm và phát sinh hiệu ứng tàn lửa, sóng nổ, âm thanh chiến trường
detectCombatImpacts :: GameState -> GameState -> ([Particle], [Shockwave], [SoundEvent])
detectCombatImpacts !sPrev !sCur =
  let !prevBulletPos = map bulletPos (bullets sPrev)
      !curBulletPos  = map bulletPos (bullets sCur)
      !explodedPos   = filter (`notElem` curBulletPos) prevBulletPos

      (!bParticles, !bShockwaves) =
        foldl' (\(!pAcc, !wAcc) bp ->
          let (!bx, !by) = toScreenPos bp
          in ( spawnSparks bx by (makeColorI 255 190 40 255) ++ pAcc
             , spawnShockwave bx by (makeColorI 0 255 240 255) : wAcc
             )
          ) ([], []) explodedPos

      -- Phát hiện xe tăng bị trúng đạn
      !hitTanks =
        [ tCur
        | (tId, tCur) <- Map.toList (tanks sCur)
        , Just tPrev <- [Map.lookup tId (tanks sPrev)]
        , tankHealth tCur < tankHealth tPrev
        , tankRespawnTimer tCur == 0
        ]

      -- Phát hiện xe tăng bị tiêu diệt (Takedown)
      !deadTanks =
        [ tCur
        | (tId, tCur) <- Map.toList (tanks sCur)
        , Just tPrev <- [Map.lookup tId (tanks sPrev)]
        , tankRespawnTimer tCur > 0 && tankRespawnTimer tPrev == 0
        ]

      -- Phát hiện kích hoạt Tuyệt Kỹ Tối Thượng [E]
      !ultTanks =
        [ tCur
        | (tId, tCur) <- Map.toList (tanks sCur)
        , Just tPrev <- [Map.lookup tId (tanks sPrev)]
        , tankEnergy tPrev >= maxEnergy && tankEnergy tCur < 20
        ]

      -- Hiệu ứng hình ảnh khi trúng đạn thông thường
      (!hitParticles, !hitShockwaves) =
        foldl' (\(!pAcc, !wAcc) tank ->
          let (!tx, !ty) = toScreenPos (tankPos tank)
          in ( spawnSparks tx ty (makeColorI 255 70 70 255) ++ pAcc
             , spawnShockwave tx ty (makeColorI 255 80 80 255) : wAcc
             )
          ) ([], []) hitTanks

      -- Hiệu ứng bộc phá hoành tráng khi tiêu diệt mục tiêu (Takedown Explosion)
      (!deadParticles, !deadShockwaves) =
        foldl' (\(!pAcc, !wAcc) tank ->
          let (!tx, !ty) = toScreenPos (tankPos tank)
          in ( spawnSparks tx ty (makeColorI 255 140 20 255)
               ++ spawnSparks tx ty (makeColorI 255 40 40 255) ++ pAcc
             , spawnCustomShockwave tx ty 58.0 (makeColorI 255 100 20 255) : wAcc
             )
          ) ([], []) deadTanks

      -- Hiệu ứng thị giác đặc trưng cho từng Tuyệt Kỹ của 5 Hero Tank
      (!ultParticles, !ultShockwaves) =
        foldl' (\(!pAcc, !wAcc) tank ->
          let (!tx, !ty) = toScreenPos (tankPos tank)
          in case tankClass tank of
               -- Vanguard Cyan: Sóng xung kích băng giá cực đại AoE 5x5
               VanguardCyan ->
                 ( spawnSparks tx ty (makeColorI 170 240 255 255) ++ pAcc
                 , spawnCustomShockwave tx ty 75.0 (makeColorI 100 240 255 255) : wAcc
                 )
               -- Titan Crimson: Chùm hỏa lực pháo kích bùng cháy
               TitanCrimson ->
                 ( spawnSparks tx ty (makeColorI 255 80 20 255) ++ pAcc
                 , spawnCustomShockwave tx ty 48.0 (makeColorI 255 120 20 255) : wAcc
                 )
               -- Viper Emerald: Sóng nano độc tố hồi sinh
               ViperEmerald ->
                 ( spawnSparks tx ty (makeColorI 40 255 100 255) ++ pAcc
                 , spawnCustomShockwave tx ty 52.0 (makeColorI 40 255 100 255) : wAcc
                 )
               -- Solaris Gold: Chớp sáng quang năng cực mạnh
               SolarisGold ->
                 ( spawnSparks tx ty (makeColorI 255 230 40 255) ++ pAcc
                 , spawnCustomShockwave tx ty 55.0 (makeColorI 255 210 20 255) : wAcc
                 )
               -- Phantom Violet: Hư không bộc phá
               PhantomViolet ->
                 ( spawnSparks tx ty (makeColorI 200 50 255 255) ++ pAcc
                 , spawnCustomShockwave tx ty 58.0 (makeColorI 190 45 255 255) : wAcc
                 )
          ) ([], []) ultTanks

      -- Danh sách âm thanh phát sinh (Ưu tiên: Takedown > Ultimate > Hit > PowerUp)
      !hasPowerUp = length (powerUps sCur) < length (powerUps sPrev)
      !soundQueue =
        if not (null deadTanks) then [SndTakedown]
        else if not (null ultTanks) then [SndUltimate]
        else if not (null hitTanks) then [SndHit]
        else if hasPowerUp then [SndPowerUp]
        else []

      !allParticles  = bParticles ++ hitParticles ++ deadParticles ++ ultParticles
      !allShockwaves = bShockwaves ++ hitShockwaves ++ deadShockwaves ++ ultShockwaves
  in (allParticles, allShockwaves, soundQueue)

spawnSparks :: Float -> Float -> Color -> [Particle]
spawnSparks !x !y !col =
  [ Particle
      { partX     = x
      , partY     = y
      , partVX    = 85.0 * cos a
      , partVY    = 85.0 * sin a
      , partLife  = 1.0
      , partDecay = 2.4 + fromIntegral (i `mod` 4) * 0.4
      , partSize  = 3.5
      , partColor = col
      }
  | i <- [0..17 :: Int]
  , let !a = fromIntegral i * (2.0 * pi / 18.0)
  ]

spawnShockwave :: Float -> Float -> Color -> Shockwave
spawnShockwave !x !y !col =
  spawnCustomShockwave x y 36.0 col

spawnCustomShockwave :: Float -> Float -> Float -> Color -> Shockwave
spawnCustomShockwave !x !y !maxR !col = Shockwave
  { shockX      = x
  , shockY      = y
  , shockRadius = 4.0
  , shockMaxR   = maxR
  , shockLife   = 1.0
  , shockColor  = col
  }

stepParticles :: Float -> [Particle] -> [Particle]
stepParticles !dt ps =
  [ p { partX = px + pvx * dt
      , partY = py + pvy * dt
      , partLife = life'
      }
  | p@(Particle px py pvx pvy life decay _ _) <- ps
  , let !life' = life - decay * dt
  , life' > 0.0
  ]

stepShockwaves :: Float -> [Shockwave] -> [Shockwave]
stepShockwaves !dt sws =
  [ sw { shockRadius = r + 95.0 * dt
       , shockLife   = life'
       }
  | sw@(Shockwave _ _ r maxR life _) <- sws
  , let !life' = life - 2.5 * dt
  , life' > 0.0 && r < maxR
  ]
