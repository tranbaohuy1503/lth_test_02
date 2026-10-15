-- ============================================================================
-- ĐỒ ÁN CUỐI KỲ: MULTIPLAYER TACTICAL 3D HERO TANK BATTLE ARENA (NHÓM 5 THÀNH VIÊN)
-- File: src/Server/State.hs
-- Phụ trách: THÀNH VIÊN 3 (Quản Lý Bộ Nhớ Chung & Nhịp Tim Game)
-- ============================================================================
-- [Chap5_Thread & Chap6_Syncronization]
-- 1. Software Transactional Memory (STM):
--    Tập trung toàn bộ GameState vào biến TVar tập trung. Tất cả các luồng mạng
--    của người chơi và Bot AI khi ghi dữ liệu đều phải thông qua khối 'atomically',
--    đảm bảo nguyên tắc ACID, loại trừ Race Condition và Deadlock.
-- 2. Strict Modification (modifyTVar'):
--    Sử dụng hàm biến đổi nghiêm ngặt kết hợp BangPatterns (!) để ép tính toán GameState
--    về WHNF ngay trong giao dịch STM, ngăn chặn rò rỉ bộ nhớ (Space Leaks).
-- 3. STM Mailbox Pattern (TChan):
--    Kênh hàng đợi TChan (Int, Command) cho phép các luồng nhận lệnh ghi bất đồng bộ
--    non-blocking, hỗ trợ cơ chế gom cụm (Write Aggregation / Batching).
-- ============================================================================

{-# LANGUAGE BangPatterns #-}

module Server.State
  ( ServerState
  , CommandQueue
  , initServerState
  , initCommandQueue
  , applyClientCommand
  , addPlayer
  , addPlayerWithClass
  , removePlayer
  , enqueueCommand
  , drainCommands
  , applyBatchCommands
  ) where

import Types
import GameLogic (updateGameState, spawnTank)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map

-- | Biến quản lý trạng thái GameState tập trung thông qua bộ nhớ giao dịch STM
type ServerState = TVar GameState

-- | Hàng đợi lưu trữ lệnh từ người chơi gửi về Server theo mô hình Mailbox STM
type CommandQueue = TChan (Int, Command)

-- | [Chap 6 STM] Khởi tạo trạng thái ban đầu cho máy chủ với bàn cờ rỗng
initServerState :: IO ServerState
initServerState =
  newTVarIO (GameState Map.empty [] Map.empty [] [] 0)

-- | [Chap 6 STM] Khởi tạo hàng đợi lệnh TChan
initCommandQueue :: IO CommandQueue
initCommandQueue =
  newTChanIO

-- | [Chap 6 STM] Đưa một lệnh của người chơi vào hàng đợi Mailbox mà không bị block luồng
enqueueCommand :: CommandQueue -> Int -> Command -> IO ()
enqueueCommand !q !pId !cmd = atomically $
  writeTChan q (pId, cmd)

-- | [Chap 6 STM] Rút sạch toàn bộ lệnh đang chờ trong hàng đợi để xử lý hàng loạt (Batching)
drainCommands :: CommandQueue -> STM [(Int, Command)]
drainCommands !q = loop []
  where
    loop !acc = do
      empty <- isEmptyTChan q
      if empty
        then return (reverse acc)
        else do
          cmd <- readTChan q
          loop (cmd : acc)

-- | [Chap 6 STM & Strictness] Áp dụng một gói nhiều lệnh đồng thời vào GameState trong 1 giao dịch STM
applyBatchCommands :: ServerState -> [(Int, Command)] -> STM ()
applyBatchCommands !stVar !batchCmds =
  modifyTVar' stVar (\(!s0) ->
    foldl' (\(!acc) (!pid, !cmd) -> updateGameState pid cmd acc) s0 batchCmds
  )

-- | [Chap 6 STM] Áp dụng trực tiếp 1 lệnh của người chơi vào TVar GameState (đồng bộ đơn lẻ)
applyClientCommand :: ServerState -> Int -> Command -> IO ()
applyClientCommand !stVar !pId !cmd = atomically $
  modifyTVar' stVar (\(!st) -> updateGameState pId cmd st)

-- | [Chap 6 STM] Cấp phát và thêm mới xe tăng của người chơi vào phòng đấu (mặc định VanguardCyan)
addPlayer :: ServerState -> Int -> IO ()
addPlayer !stVar !pId =
  addPlayerWithClass stVar pId VanguardCyan

-- | [Chap 6 STM] Cấp phát xe tăng với lớp tướng chỉ định
addPlayerWithClass :: ServerState -> Int -> TankClass -> IO ()
addPlayerWithClass !stVar !pId !cls = atomically $
  modifyTVar' stVar (\(!st) -> spawnTank pId cls st)

-- | [Chap 6 STM] Xóa xe tăng của người chơi khi ngắt kết nối
removePlayer :: ServerState -> Int -> IO ()
removePlayer !stVar !pId = atomically $
  modifyTVar' stVar (\(!st) -> updateGameState pId Disconnect st)
