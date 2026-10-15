-- ============================================================================
-- ĐỒ ÁN CUỐI KỲ: MULTIPLAYER TANK BATTLE ARENA (NHÓM 5 THÀNH VIÊN)
-- File: src/Server/Network.hs
-- Phụ trách: THÀNH VIÊN 2 (Lập Trình Mạng & Mã Hóa Gói Tin)
-- ============================================================================
-- [Chap11-Networking & Chap5_Thread]
-- 1. TCP Server Lifecycle: Khởi tạo Socket, bind cổng, lắng nghe và chấp nhận kết nối.
--    Mỗi kết nối của Client được phục vụ bởi 1 Green Thread siêu nhẹ độc lập.
-- 2. Client Management (MVar ClientMap): Quản lý an toàn danh sách Socket của các Client
--    bằng cơ chế biến đồng bộ hóa MVar (Mutual Exclusion).
-- 3. Buffered Line Receiver: Nhận luồng byte stream từ TCP, ghép mảnh và tách theo dòng '\n'
--    bảo đảm không bị dính gói tin hay mất dữ liệu khi truyền tải liên tục.
-- 4. Safe Broadcast Loop: Phát sóng trạng thái GameState tới toàn bộ Client, có cơ chế bắt
--    ngoại lệ SomeException độc lập cho từng Socket để tránh hiệu ứng domino khi 1 máy rớt mạng.
-- ============================================================================

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Network
  ( ClientMap
  , initClientMap
  , openServerSocket
  , receiveLinesLoop
  , broadcastGameState
  ) where

import Types
import Protocol (encodeGameState)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent (MVar, newMVar, readMVar)
import Control.Exception (SomeException, catch)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Control.Monad (unless)

-- | Bảng tra cứu Socket của các Client đang kết nối, bảo vệ bằng MVar
type ClientMap = MVar (Map.Map Int Socket)

-- | Khởi tạo bảng danh sách Client rỗng
initClientMap :: IO ClientMap
initClientMap = newMVar Map.empty

-- | [Chap 11] Mở Socket máy chủ TCP, thiết lập cờ ReuseAddr và lắng nghe cổng
openServerSocket :: HostName -> ServiceName -> IO Socket
openServerSocket host port = do
  let hints = defaultHints
        { addrFlags      = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  addrs <- getAddrInfo (Just hints) (Just host) (Just port)
  case addrs of
    [] -> error $ "Không thể tìm thấy địa chỉ hợp lệ cho " ++ host ++ ":" ++ port
    (serverAddr:_) -> do
      sock <- socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress serverAddr)
      listen sock 10
      putStrLn $ "[Server] Dang lang nghe tren cong " ++ port ++ " (" ++ host ++ ")..."
      return sock

-- | [Chap 11] Bộ đệm nhận dòng hoàn chỉnh từ TCP Stream (Buffered Line Receiver)
-- Tự động ghép nối các khối byte nhận được, cắt rời theo dấu '\n' và kích hoạt callback
receiveLinesLoop :: Socket -> (BS.ByteString -> IO ()) -> IO ()
receiveLinesLoop sock onLine = loop BS.empty
  where
    loop !buffer = do
      chunk <- recv sock 2048
      if BS.null chunk
        then return () -- Kết nối bị đóng từ phía đối tác (EOF)
        else do
          let !combined = BS.append buffer chunk
              rawLines  = BS.split '\n' combined
          -- Phần tử cuối cùng là phần dòng chưa kết thúc (hoặc BS.empty nếu vừa vặn '\n')
          let completeLines = init rawLines
              !remainder    = last rawLines
          mapM_ (\line -> do
            let !trimmed = BS.filter (\c -> c /= '\r') line
            unless (BS.null trimmed) (onLine trimmed)
            ) completeLines
          loop remainder

-- | [Chap 11 & Chap 5] Phát sóng trạng thái mới nhất tới toàn bộ Client
-- Bắt ngoại lệ SomeException độc lập cho từng Socket để bảo đảm tính chịu lỗi (Fault-Tolerance)
broadcastGameState :: ClientMap -> GameState -> IO ()
broadcastGameState clientMapVar !st = do
  clients <- readMVar clientMapVar
  let !payload = encodeGameState st
  mapM_ (\(!cId, !sock) ->
    catch (sendAll sock payload)
          (\(ex :: SomeException) ->
            putStrLn $ "[Server] Loi phat song toi Client " ++ show cId ++ ": " ++ show ex
          )
    ) (Map.toList clients)
