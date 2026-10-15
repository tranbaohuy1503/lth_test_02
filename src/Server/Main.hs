-- ============================================================================
-- ĐỒ ÁN CUỐI KỲ: MULTIPLAYER TANK BATTLE ARENA (NHÓM 5 THÀNH VIÊN)
-- File: src/Server/Main.hs
-- Phụ trách: Tích hợp hệ thống Máy Chủ (Server Main Coordinator)
-- ============================================================================
-- [Chap11-Networking & Chap5_Thread & Chap7_ConcurrencySupervision]
-- 1. Lifecycle Management: Sử dụng 'bracket' để khởi tạo và đóng Server Socket an toàn,
--    chống rò rỉ cổng mạng (Port Leaks) khi chương trình bị tắt ngang.
-- 2. Concurrency Orchestration: Điều phối 3 thành phần luồng đồng thời:
--    - Luồng Green Thread nhịp tim thế giới 30 FPS (runTickLoop).
--    - Luồng Green Thread Bot AI tự hành có giám sát tự khởi động lại (superviseBotWorker).
--    - Vòng lặp lắng nghe kết nối (accept), cấp phát Client ID tuần tự và bọc từng kết nối
--      trong cây giám sát luồng độc lập (superviseClient).
-- ============================================================================

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Protocol (decodeCommand, encodeClientId)
import Server.State (initServerState, initCommandQueue, enqueueCommand)
import Server.Network (initClientMap, openServerSocket, receiveLinesLoop)
import Server.Tick (runTickLoop)
import Server.Supervisor (superviseClient, superviseBotWorker)
import AI (runBotLoop)
import Network.Socket (accept, close)
import Network.Socket.ByteString (sendAll)
import Control.Concurrent (forkIO, newMVar, modifyMVar)
import Control.Exception (bracket)
import System.Environment (getArgs)
import System.IO (hSetEncoding, hSetBuffering, stdout, stderr, utf8, BufferMode(LineBuffering))
import Control.Monad (forever)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  let port = case args of
        (p:_) -> p
        []    -> "9123"
      host = "0.0.0.0"

  putStrLn "============================================================"
  putStrLn "   MULTIPLAYER TANK BATTLE ARENA - HASKELL FINAL PROJECT   "
  putStrLn "   SERVER TRUYEN THONG REALTIME (TCP CLIENT-SERVER 30 FPS) "
  putStrLn "============================================================"
  putStrLn $ "[Server] Dang khoi tao va lang nghe tren cong: " ++ port

  -- [Chap 6 STM] Khởi tạo bộ nhớ dùng chung GameState và hàng đợi lệnh Mailbox TChan
  stVar    <- initServerState
  cmdQueue <- initCommandQueue

  -- [Chap 5 & Chap 11] Khởi tạo bảng ánh xạ Client MVar và bộ đếm ID người chơi
  clientMap       <- initClientMap
  clientIdCounter <- newMVar (1 :: Int)

  -- [Chap 5 Green Thread] Khởi chạy luồng nhịp tim thế giới 30 FPS
  _ <- forkIO $ do
    putStrLn "[Server] Khoi chay luong Game Tick Loop (30 FPS)..."
    runTickLoop stVar cmdQueue clientMap

  -- [Chap 7 Concurrency Supervision] Khởi chạy Bot AI tự động có cơ chế tự phục hồi
  _ <- forkIO $ do
    putStrLn "[Server] Khoi chay Bot AI Worker (ID 999) co giam sat..."
    superviseBotWorker "Bot-Tank-999" (runBotLoop 999 cmdQueue stVar)

  -- [Chap 11 & Exception bracket] Mở socket máy chủ và đảm bảo đóng cổng khi thoát
  bracket (openServerSocket host port) close $ \serverSock -> do
    putStrLn $ "[Server] San sang phuc vu ket noi tai cong " ++ port
    putStrLn "============================================================"

    -- Vòng lặp chấp nhận kết nối người chơi
    forever $ do
      (clientSock, clientAddr) <- accept serverSock

      -- Cấp phát mã định danh người chơi tuần tự (1, 2, 3...)
      assignedId <- modifyMVar clientIdCounter $ \curId ->
        return (curId + 1, curId)

      putStrLn $ "[Server] Chap nhan ket noi tu " ++ show clientAddr
              ++ " -> Gan Player ID: " ++ show assignedId

      -- [Chap 11 Framing] Gửi ngay mã ID vừa cấp phát cho Client biết
      sendAll clientSock (encodeClientId assignedId)

      -- [Chap 5 Green Thread & Chap 7 Supervision] Tạo luồng phục vụ riêng cho người chơi
      _ <- forkIO $ superviseClient assignedId clientSock clientAddr stVar clientMap $ do
        -- Lắng nghe từng dòng lệnh hoàn chỉnh từ Client và đẩy vào hàng đợi STM
        receiveLinesLoop clientSock $ \lineBs -> do
          case decodeCommand lineBs of
            Right cmd ->
              enqueueCommand cmdQueue assignedId cmd
            Left err  ->
              putStrLn $ "[Server] Goi tin khong hop le tu Client "
                      ++ show assignedId ++ ": " ++ err

      return ()
