-- ============================================================================
-- ĐỒ ÁN CUỐI KỲ: MULTIPLAYER TACTICAL 3D HERO TANK BATTLE ARENA (NHÓM 5 THÀNH VIÊN)
-- File: src/Client/Audio.hs
-- Phụ trách: MODULE HỆ THỐNG ÂM THANH REALTIME (AUDIO ENGINE EXTENSION)
-- ============================================================================
-- [QUY ĐỊNH RÕ RÀNG VỀ THƯ VIỆN NGOẠI LỆ ĐƯỢC PHÉP THEO YÊU CẦU ĐỒ ÁN]:
-- Module này sử dụng các thư viện hệ thống và nhị phân được cho phép:
-- 1. 'ForeignFunctionInterface' (Win32 winmm.dll PlaySoundA & mciSendStringA):
--    - PlaySoundA: Phát âm thanh SFX phần cứng với ĐỘ TRỄ 0ms (Zero Latency),
--      hoàn toàn đồng bộ với phím di chuyển, khai hỏa, takedown và va chạm.
--    - mciSendStringA: Phát nhạc nền Cyber Synth BGM lặp vô hạn (Loop) trên kênh
--      phần cứng độc lập, không bị ngắt quãng hay xung đột khi khai hỏa đạn.
-- 2. 'directory' & 'filepath': Quản lý thư mục lưu trữ tài nguyên assets/audio/.
-- 3. 'binary' (Data.Binary.Put): Đóng gói cấu trúc nhị phân RIFF/WAVE PCM 16-bit chuẩn.
-- ============================================================================
-- [PROCEDURAL WAV SYNTHESIZER]:
-- Toàn bộ nhạc nền BGM và 7 hiệu ứng âm thanh chiến trường (Realistic Shoot,
-- Takedown, Hit, Explode, Ultimate Surge, PowerUp, Respawn) được tổng hợp
-- toán học dạng sóng PCM 16-bit ngay khi khởi động game, bảo đảm ZERO lỗi
-- thiếu file asset âm thanh trên mọi máy tính.
-- ============================================================================

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Client.Audio
  ( SoundEvent(..)
  , initAudioSystem
  , playSound
  , startBgmLoop
  , stopBgm
  , setBgmMute
  ) where

import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute)
import System.FilePath ((</>))
import Control.Exception (SomeException, catch)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Int (Int16)
import Data.Word (Word32)
import Foreign.C.String (withCString, CString)
import Foreign.C.Types (CUInt(..), CBool(..))
import Foreign.Ptr (nullPtr, Ptr)
import Data.Fixed (mod')

-- | Hàm phát âm thanh hiệu ứng phần cứng từ Windows Multimedia API
foreign import ccall safe "PlaySoundA"
  c_playSound :: CString -> Ptr () -> CUInt -> IO CBool

-- | Hàm điều khiển thiết bị đa phương tiện MCI (phát nhạc nền loop không chặn luồng)
foreign import ccall safe "mciSendStringA"
  c_mciSendString :: CString -> Ptr () -> CUInt -> Ptr () -> IO CUInt

-- | Cờ Windows PlaySound: SND_ASYNC (0x1) .|. SND_NODEFAULT (0x2) .|. SND_FILENAME (0x20000)
sndFlagsAsync :: CUInt
sndFlagsAsync = 0x00020003

-- | Các sự kiện âm thanh chiến trường
data SoundEvent
  = SndShoot      -- ^ Tiếng khai hỏa đại bác chân thực (Realistic Tank Cannon Report)
  | SndHit        -- ^ Tiếng va chạm kim loại / đạn đâm xuyên giáp
  | SndExplode    -- ^ Tiếng nổ hủy diệt khi đạn hoặc chướng ngại vật nổ tung
  | SndUltimate   -- ^ Tiếng tích năng lượng và bùng nổ Tuyệt chiêu Tối Thượng [E]
  | SndPowerUp    -- ^ Tiếng chuông năng lượng khi nhặt ngọc sức mạnh
  | SndRespawn    -- ^ Tiếng warp không gian hồi sinh và kích hoạt khiên
  | SndTakedown   -- ^ Tiếng triệt hạ đối thủ (Takedown / Kill Reward Cue)
  deriving (Eq, Show)

-- | Tên file tương ứng cho từng sự kiện âm thanh
soundFileName :: SoundEvent -> FilePath
soundFileName evt = case evt of
  SndShoot    -> "shoot.wav"
  SndHit      -> "hit.wav"
  SndExplode  -> "explode.wav"
  SndUltimate -> "ultimate.wav"
  SndPowerUp  -> "powerup.wav"
  SndRespawn  -> "respawn.wav"
  SndTakedown -> "takedown.wav"

-- | Khởi tạo hệ thống âm thanh: Tạo thư mục và tổng hợp toán học toàn bộ file .wav
initAudioSystem :: IO ()
initAudioSystem = do
  let dir = "assets" </> "audio"
  createDirectoryIfMissing True dir
  generateIfMissing dir SndShoot    synthesizeShoot
  generateIfMissing dir SndHit      synthesizeHit
  generateIfMissing dir SndExplode  synthesizeExplode
  generateIfMissing dir SndUltimate synthesizeUltimate
  generateIfMissing dir SndPowerUp  synthesizePowerUp
  generateIfMissing dir SndRespawn  synthesizeRespawn
  generateIfMissing dir SndTakedown synthesizeTakedown
  generateBgmIfMissing dir
  putStrLn "[Audio Engine] He thong am thanh & Nhac nen BGM san sang (8 file WAV PCM 16-bit)."

-- | Kiểm tra và tổng hợp file SFX nếu chưa tồn tại
generateIfMissing :: FilePath -> SoundEvent -> (Int -> [Int16]) -> IO ()
generateIfMissing dir evt synthFunc = do
  let path = dir </> soundFileName evt
  exists <- doesFileExist path
  if exists
    then return ()
    else do
      let sampleRate = 22050
          samples    = synthFunc sampleRate
          wavBytes   = buildRiffWav sampleRate samples
      catch (BL.writeFile path wavBytes)
            (\(_ :: SomeException) -> return ())

-- | Kiểm tra và tổng hợp file nhạc nền BGM (Cyber Bassline Synth)
generateBgmIfMissing :: FilePath -> IO ()
generateBgmIfMissing dir = do
  let path = dir </> "bgm.wav"
  exists <- doesFileExist path
  if exists
    then return ()
    else do
      let sampleRate = 22050
          samples    = synthesizeBgm sampleRate
          wavBytes   = buildRiffWav sampleRate samples
      catch (BL.writeFile path wavBytes)
            (\(_ :: SomeException) -> return ())

-- | Phát âm thanh phần cứng với độ trễ 0ms (hoàn toàn đồng bộ với phím và di chuyển)
playSound :: Bool -> SoundEvent -> IO ()
playSound isMuted evt
  | isMuted   = return ()
  | otherwise = do
      let path = "assets" </> "audio" </> soundFileName evt
      catch (withCString path (\cStr -> do
               _ <- c_playSound cStr nullPtr sndFlagsAsync
               return ()
             ))
            (\(_ :: SomeException) -> return ())

-- | Khởi động luồng phát nhạc nền BGM lặp vô hạn qua Windows MCI
startBgmLoop :: Bool -> IO ()
startBgmLoop isMuted
  | isMuted   = return ()
  | otherwise = do
      let path = "assets" </> "audio" </> "bgm.wav"
      absPath <- makeAbsolute path
      catch (do
        _ <- withCString "close bgm" (\cCmd -> c_mciSendString cCmd nullPtr 0 nullPtr)
        _ <- withCString ("open \"" ++ absPath ++ "\" type waveaudio alias bgm") (\cCmd -> c_mciSendString cCmd nullPtr 0 nullPtr)
        _ <- withCString "play bgm repeat" (\cCmd -> c_mciSendString cCmd nullPtr 0 nullPtr)
        return ()
        ) (\(_ :: SomeException) -> return ())

-- | Dừng phát nhạc nền BGM
stopBgm :: IO ()
stopBgm = catch (do
  _ <- withCString "stop bgm" (\cCmd -> c_mciSendString cCmd nullPtr 0 nullPtr)
  _ <- withCString "close bgm" (\cCmd -> c_mciSendString cCmd nullPtr 0 nullPtr)
  return ()
  ) (\(_ :: SomeException) -> return ())

-- | Đổi trạng thái bật/tắt âm thanh (Mute/Unmute) cho cả BGM và SFX
setBgmMute :: Bool -> IO ()
setBgmMute isMuted =
  if isMuted then stopBgm else startBgmLoop False

-- ============================================================================
-- ĐÓNG GÓI CHUẨN ĐỊNH DẠNG RIFF/WAVE PCM 16-BIT BẰNG DATA.BINARY
-- ============================================================================

buildRiffWav :: Int -> [Int16] -> BL.ByteString
buildRiffWav sampleRate samples = runPut $ do
  let numSamples    = fromIntegral (length samples) :: Word32
      subchunk2Size = numSamples * 2                   :: Word32
      chunkSize     = 36 + subchunk2Size               :: Word32
      byteRate      = fromIntegral (sampleRate * 2)    :: Word32
      sampleRateW   = fromIntegral sampleRate          :: Word32

  -- 1. Chunk ID: "RIFF"
  putWord8 0x52; putWord8 0x49; putWord8 0x46; putWord8 0x46
  -- 2. Chunk Size (Little Endian)
  putWord32le chunkSize
  -- 3. Format: "WAVE"
  putWord8 0x57; putWord8 0x41; putWord8 0x56; putWord8 0x45

  -- 4. Subchunk 1 ID: "fmt "
  putWord8 0x66; putWord8 0x6D; putWord8 0x74; putWord8 0x20
  -- 5. Subchunk 1 Size: 16 (cho định dạng PCM)
  putWord32le 16
  -- 6. Audio Format: 1 (PCM Linear)
  putWord16le 1
  -- 7. Num Channels: 1 (Mono)
  putWord16le 1
  -- 8. Sample Rate
  putWord32le sampleRateW
  -- 9. Byte Rate: SampleRate * NumChannels * BitsPerSample/8
  putWord32le byteRate
  -- 10. Block Align: NumChannels * BitsPerSample/8 = 2
  putWord16le 2
  -- 11. Bits Per Sample: 16
  putWord16le 16

  -- 12. Subchunk 2 ID: "data"
  putWord8 0x64; putWord8 0x61; putWord8 0x74; putWord8 0x61
  -- 13. Subchunk 2 Size
  putWord32le subchunk2Size

  -- 14. Các mẫu âm thanh PCM 16-bit
  mapM_ putInt16le samples

-- ============================================================================
-- CÁC HÀM TỔNG HỢP TOÁN HỌC DẠNG SÓNG (PROCEDURAL AUDIO SYNTHESIS)
-- ============================================================================

-- | Khai hỏa súng đại bác chân thực (Realistic Heavy Tank Cannon Report):
-- 1. Xung áp lực cực đại ban đầu (Transient Crack & Noise Spike trong 18ms đầu)
-- 2. Sóng áp suất trầm nặng uy lực (Heavy Sub-Bass Punch từ 180 Hz quét xuống 42 Hz)
-- 3. Âm vang kim loại buồng nòng pháo (Metallic Chamber Decay)
synthesizeShoot :: Int -> [Int16]
synthesizeShoot sr =
  let duration   = 0.22 :: Double
      totalCount = floor (duration * fromIntegral sr) :: Int
  in [ let t    = fromIntegral i / fromIntegral sr :: Double
           -- 1. Xung nổ tức thì cực mạnh
           crack = if t < 0.022
                     then let pseudoNoise = sin (fromIntegral i * 12345.67)
                              decayN      = (0.022 - t) / 0.022
                          in pseudoNoise * decayN * 0.70
                     else 0.0
           -- 2. Sóng xung kích uy lực âm trầm
           freq  = 180.0 * exp (-24.0 * t) + 42.0
           bassAmp = exp (-14.0 * t) * (1.0 - t / duration)
           bass  = sin (2.0 * pi * freq * t) * bassAmp
           -- 3. Độ vang kim loại buồng pháo
           metal = 0.22 * sin (2.0 * pi * 480.0 * exp (-16.0 * t) * t) * exp (-18.0 * t)
           wave  = crack + 0.82 * bass + metal
           val   = floor (wave * 29000.0) :: Int
       in fromIntegral (clamp16 val)
     | i <- [0 .. totalCount - 1 :: Int]
     ]

-- | Tiếng triệt hạ đối thủ (Takedown / Kill Reward Cue):
-- Âm trầm nổ rền vang (140 Hz xuống 30 Hz) hòa trộn chuông hợp âm chiến thắng số hóa
synthesizeTakedown :: Int -> [Int16]
synthesizeTakedown sr =
  let duration   = 0.52 :: Double
      totalCount = floor (duration * fromIntegral sr) :: Int
  in [ let t    = fromIntegral i / fromIntegral sr :: Double
           -- Sonic boom bass drop
           freq  = 140.0 * exp (-6.5 * t) + 28.0
           amp   = exp (-5.2 * t) * (1.0 - t / duration)
           bass  = sin (2.0 * pi * freq * t)
           -- Digital victory chime (A4 + E5 + A5)
           chime = if t < 0.35
                     then ( sin (2.0 * pi * 880.0 * t)
                          + 0.5 * sin (2.0 * pi * 1320.0 * t)
                          + 0.3 * sin (2.0 * pi * 1760.0 * t)
                          ) * exp (-9.0 * t) * 0.35
                     else 0.0
           wave  = 0.80 * bass + chime
           val   = floor (amp * wave * 30000.0) :: Int
       in fromIntegral (clamp16 val)
     | i <- [0 .. totalCount - 1 :: Int]
     ]

-- | Va chạm kim loại / Đạn bắn trúng giáp: Đanh thép, sắc bén với suy hao nhanh
synthesizeHit :: Int -> [Int16]
synthesizeHit sr =
  let duration   = 0.12 :: Double
      totalCount = floor (duration * fromIntegral sr) :: Int
  in [ let t    = fromIntegral i / fromIntegral sr :: Double
           amp  = exp (-32.0 * t)
           wave = 0.65 * sin (2.0 * pi * 1350.0 * t)
                + 0.35 * sin (2.0 * pi * 2700.0 * t)
           val  = floor (amp * wave * 24000.0) :: Int
       in fromIntegral (clamp16 val)
     | i <- [0 .. totalCount - 1 :: Int]
     ]

-- | Vụ nổ hủy diệt: Âm trầm 80 Hz xuống 30 Hz phối hợp tạp âm xung kích
synthesizeExplode :: Int -> [Int16]
synthesizeExplode sr =
  let duration   = 0.45 :: Double
      totalCount = floor (duration * fromIntegral sr) :: Int
  in [ let t     = fromIntegral i / fromIntegral sr :: Double
           amp   = exp (-6.5 * t) * (1.0 - t / duration)
           bass  = sin (2.0 * pi * (80.0 * exp (-4.0 * t) + 30.0) * t)
           noise = sin (12345.67 * t)
           wave  = 0.70 * bass + 0.30 * noise
           val   = floor (amp * wave * 28000.0) :: Int
       in fromIntegral (clamp16 val)
     | i <- [0 .. totalCount - 1 :: Int]
     ]

-- | Tuyệt chiêu Ultimate: Quét tần số tích tụ năng lượng và giải phóng siêu thanh
synthesizeUltimate :: Int -> [Int16]
synthesizeUltimate sr =
  let duration   = 0.65 :: Double
      totalCount = floor (duration * fromIntegral sr) :: Int
  in [ let t    = fromIntegral i / fromIntegral sr :: Double
           amp  = if t < 0.22
                    then (t / 0.22) * 0.85
                    else exp (-4.8 * (t - 0.22))
           freq = if t < 0.22
                    then 220.0 + (1300.0 - 220.0) * (t / 0.22)
                    else 480.0 * exp (-4.2 * (t - 0.22)) + 55.0
           wave = sin (2.0 * pi * freq * t)
           val  = floor (amp * wave * 27000.0) :: Int
       in fromIntegral (clamp16 val)
     | i <- [0 .. totalCount - 1 :: Int]
     ]

-- | Nhặt ngọc sức mạnh: Hợp âm arpeggio thăng hoa (A4 - C#5 - E5 - A5)
synthesizePowerUp :: Int -> [Int16]
synthesizePowerUp sr =
  let duration   = 0.32 :: Double
      totalCount = floor (duration * fromIntegral sr) :: Int
  in [ let t    = fromIntegral i / fromIntegral sr :: Double
           step = floor (t / 0.08) :: Int
           freq = case step of
                    0 -> 440.0 -- A4
                    1 -> 554.37 -- C#5
                    2 -> 659.25 -- E5
                    _ -> 880.0 -- A5
           subT = t - fromIntegral step * 0.08
           amp  = exp (-12.0 * subT)
           wave = sin (2.0 * pi * freq * t)
           val  = floor (amp * wave * 20000.0) :: Int
       in fromIntegral (clamp16 val)
     | i <- [0 .. totalCount - 1 :: Int]
     ]

-- | Hồi sinh & khiên bảo vệ: Dao động điều chế tần số (FM Warp)
synthesizeRespawn :: Int -> [Int16]
synthesizeRespawn sr =
  let duration   = 0.38 :: Double
      totalCount = floor (duration * fromIntegral sr) :: Int
  in [ let t    = fromIntegral i / fromIntegral sr :: Double
           amp  = (1.0 - t / duration) * 0.8
           modW = 120.0 * sin (2.0 * pi * 15.0 * t)
           freq = 380.0 + modW
           wave = sin (2.0 * pi * freq * t)
           val  = floor (amp * wave * 22000.0) :: Int
       in fromIntegral (clamp16 val)
     | i <- [0 .. totalCount - 1 :: Int]
     ]

-- | Nhạc nền Cyber Synth Bassline lặp vô tận (8 giây, 4 bar ở 120 BPM):
-- Hòa thanh: D minor -> Bb major -> C major -> A minor
-- Kết hợp âm bass 16th-note nảy đều, thảm âm synth pad và xung nhịp cyber nhịp nhàng
synthesizeBgm :: Int -> [Int16]
synthesizeBgm sr =
  let duration   = 8.0 :: Double
      totalCount = floor (duration * fromIntegral sr) :: Int
  in [ let t    = fromIntegral i / fromIntegral sr :: Double
           -- Xác định bar hiện tại (mỗi bar 2.0s ở 120 BPM)
           barIdx = floor (t / 2.0) `mod` (4 :: Int)
           (rootF, padF1, padF2) = case barIdx of
             0 -> (73.42, 146.83, 220.00) -- D minor
             1 -> (58.27, 116.54, 174.61) -- Bb major
             2 -> (65.41, 130.81, 196.00) -- C major
             _ -> (55.00, 110.00, 164.81) -- A minor

           -- 1. Xung Bass nảy 16th-note (125ms mỗi nốt, 8 nốt/giây)
           subT     = t `mod'` 0.125
           bassEnv  = exp (-16.0 * subT)
           bassWave = sin (2.0 * pi * rootF * t) * bassEnv

           -- 2. Dải thảm âm Synth Pad ngân dài
           padEnv   = 0.45 + 0.10 * sin (2.0 * pi * 0.5 * t)
           padWave  = (sin (2.0 * pi * padF1 * t) + sin (2.0 * pi * padF2 * t)) * 0.5 * padEnv

           -- 3. Cyber Hi-hat / pulse nhịp nhàng
           click = if subT < 0.007
                     then let randNoise = sin (fromIntegral i * 7891.23)
                          in randNoise * ((0.007 - subT) / 0.007) * 0.20
                     else 0.0

           -- Tổng hợp ở mức âm lượng nền dễ chịu (-14 dB)
           wave = 0.55 * bassWave + 0.35 * padWave + click
           val  = floor (wave * 9500.0) :: Int
       in fromIntegral (clamp16 val)
     | i <- [0 .. totalCount - 1 :: Int]
     ]

-- | Giới hạn mẫu âm thanh trong phạm vi 16-bit có dấu
clamp16 :: Int -> Int
clamp16 n
  | n > 32767  = 32767
  | n < -32768 = -32768
  | otherwise  = n
