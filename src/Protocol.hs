-- ============================================================================
-- ĐỒ ÁN CUỐI KỲ: MULTIPLAYER TACTICAL 3D HERO TANK BATTLE ARENA (NHÓM 5 THÀNH VIÊN)
-- File: src/Protocol.hs
-- Phụ trách: THÀNH VIÊN 2 (Lập Trình Mạng & Mã Hóa Gói Tin)
-- ============================================================================
-- [LTH_Chuong_7_Evaluation_Functor_Applicative_Monad]
-- 1. Monad Either String (Monadic Validation Pipeline): Xâu chuỗi quy trình giải mã:
--    ByteString thô -> parseRaw (Lọc rác '\r') -> readMaybe (Phân tích cú pháp Haskell)
--    -> validateCommand / validateGameState (Kiểm tra ngữ nghĩa biên lưới và logic).
-- 2. Line-Based Message Framing: Gói tin qua luồng TCP được bao đóng bằng ký tự '\n'
--    giúp phân tách các thông điệp dính liền (TCP packet concatenation) và bảo đảm tính toàn vẹn.
-- ============================================================================

{-# LANGUAGE BangPatterns #-}

module Protocol
  ( encodeCommand
  , decodeCommand
  , encodeGameState
  , decodeGameState
  , encodeClientId
  , decodeClientId
  , parseRaw
  , validateCommand
  , validateGameState
  ) where

import Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)

-- ============================================================================
-- KHÂU 1: LÀM SẠCH VÀ PHÂN TÁCH DÒNG (RAW PARSING)
-- ============================================================================

-- | [Chap 11] Làm sạch gói tin ByteString, gỡ bỏ ký tự điều khiển '\r' và kiểm tra rỗng
parseRaw :: BS.ByteString -> Either String String
parseRaw !bs =
  let cleanStr = BS.unpack (BS.filter (\c -> c /= '\r' && c /= '\n') bs)
  in if null cleanStr
       then Left "Lỗi giao thức: Gói tin rỗng hoặc chỉ chứa ký tự xuống dòng!"
       else Right cleanStr

-- ============================================================================
-- MÃ HÓA & GIẢI MÃ LỆNH ĐIỀU KHIỂN (COMMAND PROTOCOL)
-- ============================================================================

-- | [Chap 11 Framing] Đóng gói Command thành ByteString kèm dấu ngắt dòng '\n'
encodeCommand :: Command -> BS.ByteString
encodeCommand !cmd =
  BS.snoc (BS.pack (show cmd)) '\n'

-- | [Chap 7 Either Monad] Giải mã Command với Monadic Pipeline và kiểm tra tính hợp lệ
decodeCommand :: BS.ByteString -> Either String Command
decodeCommand !bs = do
  rawStr <- parseRaw bs
  case readMaybe rawStr of
    Just cmd -> validateCommand cmd
    Nothing  -> Left $ "Lỗi cú pháp gói tin Command: Không thể phân tích '" ++ rawStr ++ "'"

-- | Kiểm tra ngữ nghĩa của Command
validateCommand :: Command -> Either String Command
validateCommand !cmd = case cmd of
  Move d       -> Right (Move d)
  Shoot        -> Right Shoot
  CastUltimate -> Right CastUltimate
  SendChat msg ->
    if length msg > 100
      then Left "Tin nhắn trò chuyện vượt quá giới hạn 100 ký tự!"
      else Right (SendChat (take 48 msg))
  SelectClass c -> Right (SelectClass c)
  Disconnect   -> Right Disconnect

-- ============================================================================
-- MÃ HÓA & GIẢI MÃ TRẠNG THÁI TOÀN BỘ GAME (GAMESTATE PROTOCOL)
-- ============================================================================

-- | [Chap 11 Framing] Đóng gói GameState thành ByteString kèm dấu ngắt dòng '\n'
encodeGameState :: GameState -> BS.ByteString
encodeGameState !st =
  BS.snoc (BS.pack (show st)) '\n'

-- | [Chap 7 Either Monad] Giải mã GameState với Monadic Pipeline
decodeGameState :: BS.ByteString -> Either String GameState
decodeGameState !bs = do
  rawStr <- parseRaw bs
  case readMaybe rawStr of
    Just st -> validateGameState st
    Nothing -> Left "Lỗi cú pháp gói tin GameState: Cấu trúc trạng thái bị hỏng hoặc thiếu trường!"

-- | [Chap 7 Validation] Kiểm tra tính hợp lệ của GameState (tọa độ trong phạm vi 32x22)
validateGameState :: GameState -> Either String GameState
validateGameState !st = do
  let allTanks   = Map.elems (tanks st)
      allBullets = bullets st
      validPos (Pos x y) = x >= 0 && x < gridWidth && y >= 0 && y < gridHeight

      invalidTanks   = filter (\t -> not (validPos (tankPos t))) allTanks
      invalidBullets = filter (\b -> not (validPos (bulletPos b))) allBullets

  if not (null invalidTanks)
    then Left "Lỗi ngữ nghĩa GameState: Tồn tại xe tăng có tọa độ nằm ngoài bàn cờ!"
    else if not (null invalidBullets)
      then Left "Lỗi ngữ nghĩa GameState: Tồn tại viên đạn có tọa độ nằm ngoài bàn cờ!"
      else Right st

-- ============================================================================
-- THÔNG ĐIỆP BẮT TAY GÁN ID NGƯỜI CHƠI (CLIENT ID HANDSHAKE)
-- ============================================================================

-- | Mã hóa gói tin cấp phát ID cho Client khi vừa kết nối thành công
encodeClientId :: Int -> BS.ByteString
encodeClientId !cId =
  BS.snoc (BS.pack ("CLIENT_ID " ++ show cId)) '\n'

-- | Giải mã nhận dạng ID người chơi
decodeClientId :: BS.ByteString -> Maybe Int
decodeClientId !bs =
  case BS.words (BS.filter (/= '\r') bs) of
    [tag, idBs] | tag == BS.pack "CLIENT_ID" -> readMaybe (BS.unpack idBs)
    _                                        -> Nothing
