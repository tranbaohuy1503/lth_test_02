-- ============================================================================
-- ĐỒ ÁN CUỐI KỲ: MULTIPLAYER TANK BATTLE ARENA (NHÓM 5 THÀNH VIÊN)
-- File: src/Server/Supervisor.hs
-- Phụ trách: THÀNH VIÊN 4 (Giám Sát Luồng Chống Rớt Mạng & Bot AI)
-- ============================================================================
-- [Chap7_ConcurrencySupervision]
-- 1. Supervision Tree & Thread Isolation: Bọc từng luồng Client bằng 'withAsync' và 'waitCatch'.
--    Khi có ngoại lệ rớt mạng, luồng Supervisor cô lập sự cố, tự động dọn dẹp tài nguyên
--    (xóa xe tăng khỏi ServerState, gỡ Socket khỏi ClientMap, đóng cổng kết nối an toàn).
-- 2. Idle Timeout Detection (System.Timeout.timeout): Phát hiện các Client bị treo mạng hoặc
--    đứt dây cáp ngầm mà không gửi gói tin trong khoảng thời gian định trước.
-- 3. Supervisor Restart Loop: Giám sát luồng Bot AI Worker; nếu Bot gặp ngoại lệ bất thường,
--    Supervisor tự động ghi log lỗi, chờ thời gian hồi phục (backoff) và khởi động lại luồng Bot.
-- ============================================================================

{-# LANGUAGE ScopedTypeVariables #-}

module Server.Supervisor
  ( superviseClient
  , withClientTimeout
  , superviseBotWorker
  ) where

import Server.State (ServerState, addPlayer, removePlayer)
import Server.Network (ClientMap)
import Network.Socket (Socket, SockAddr, close)
import Control.Concurrent.Async (async, waitCatch, withAsync)
import Control.Concurrent (modifyMVar_, threadDelay)
import Control.Exception (SomeException, catch, finally)
import qualified System.Timeout as Timeout
import qualified Data.Map.Strict as Map

-- | [Chap 7] Giám sát luồng kết nối mạng của 1 người chơi
-- Đảm bảo an toàn tài nguyên: Đăng ký khi vào -> Giám sát trong lúc chơi -> Dọn sạch khi thoát
superviseClient
  :: Int          -- ^ Mã định danh người chơi được gán
  -> Socket       -- ^ Socket kết nối của Client
  -> SockAddr     -- ^ Địa chỉ IP / Port của Client
  -> ServerState  -- ^ Biến trạng thái máy chủ STM
  -> ClientMap    -- ^ Bảng danh sách socket kết nối MVar
  -> IO ()        -- ^ Hành động xử lý mạng chính của Client
  -> IO ()
superviseClient !cId !sock !addr !stVar !clientMapVar !clientAction = do
  putStrLn $ "[Supervisor] Bat dau giam sat Client " ++ show cId ++ " tu dia chi " ++ show addr

  -- 1. Đăng ký thông tin vào ClientMap và sinh xe tăng trong GameState
  modifyMVar_ clientMapVar (\m -> return (Map.insert cId sock m))
  addPlayer stVar cId

  -- 2. Giám sát bằng withAsync và waitCatch để bắt sạch ngoại lệ
  let monitoredAction = do
        res <- withAsync clientAction waitCatch
        case res of
          Left (ex :: SomeException) ->
            putStrLn $ "[Supervisor] Client " ++ show cId ++ " phat sinh su co: " ++ show ex
          Right () ->
            putStrLn $ "[Supervisor] Client " ++ show cId ++ " da ket thuc phien lam viec binh thuong."

  -- 3. Khối finally bảo đảm dọn dẹp tài nguyên kể cả khi luồng cha bị hủy
  monitoredAction `finally` cleanupClient
  where
    cleanupClient = do
      putStrLn $ "[Supervisor] Dang don dep tai nguyen cho Client " ++ show cId ++ "..."
      removePlayer stVar cId
      modifyMVar_ clientMapVar (\m -> return (Map.delete cId m))
      catch (close sock) (\(_ :: SomeException) -> return ())
      putStrLn $ "[Supervisor] Hoan tat don dep cho Client " ++ show cId

-- | [Chap 7 Timeout] Bao bọc một hành động IO với thời gian chờ tối đa (tính theo microsecond)
-- Trả về Nothing nếu hết hạn timeout, Just kết quả nếu thành công trước hạn
withClientTimeout :: Int -> IO a -> IO (Maybe a)
withClientTimeout !microsecs !action =
  Timeout.timeout microsecs action

-- | [Chap 7 Supervisor Restart Loop] Giám sát và tự động khởi động lại tiến trình Bot AI
-- Nếu Bot gặp ngoại lệ bất ngờ, luồng giám sát sẽ bắt lỗi, chờ 0.5s rồi hồi sinh lại Bot
superviseBotWorker :: String -> IO () -> IO ()
superviseBotWorker !workerName !workerAction = loop
  where
    loop = do
      putStrLn $ "[Supervisor] Khoi dong tien trinh worker: " ++ workerName
      workerHandle <- async workerAction
      result <- waitCatch workerHandle
      case result of
        Left (ex :: SomeException) -> do
          putStrLn $ "[Supervisor] CANH BAO: Tien trinh " ++ workerName ++ " bi sap voi loi: " ++ show ex
          putStrLn $ "[Supervisor] Tu dong khoi dong lai sau 500ms (Restart Loop)..."
          threadDelay 500000 -- Chờ 0.5 giây trước khi tái khởi động
          loop
        Right () ->
          putStrLn $ "[Supervisor] Tien trinh " ++ workerName ++ " da ket thuc sach se."
