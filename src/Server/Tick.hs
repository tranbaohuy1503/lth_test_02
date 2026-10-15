-- ============================================================================
-- ĐỒ ÁN CUỐI KỲ: MULTIPLAYER TACTICAL 3D HERO TANK BATTLE ARENA (NHÓM 5 THÀNH VIÊN)
-- File: src/Server/Tick.hs
-- Phụ trách: THÀNH VIÊN 3 (Quản Lý Bộ Nhớ Chung & Nhịp Tim Game)
-- ============================================================================
-- [Chap5_Thread & Chap6_Syncronization]
-- 1. Heartbeat Loop (Nhịp tim trò chơi 30 FPS): Sử dụng hàm threadDelay 33333 để duy trì
--    chu kỳ cập nhật chính xác 30 khung hình/giây trên luồng Green Thread độc lập.
-- 2. Write Aggregation / Batching Pattern: Gom cụm toàn bộ lệnh tích lũy trong TChan
--    và thực thi đồng thời với hàm stepGameTick trong MỘT giao dịch atomically duy nhất.
-- 3. Dynamic Power-Up Spawner: Cứ mỗi 120 ticks (~4 giây), tự động kiểm tra và sinh
--    ngọc sức mạnh 3D (HealOrb, EnergyOrb, OverchargeOrb) tại các tọa độ chiến thuật.
-- ============================================================================

{-# LANGUAGE BangPatterns #-}

module Server.Tick
  ( runTickLoop
  ) where

import Types
import GameLogic (stepGameTick, updateGameState, spawnPowerUp)
import Server.State (ServerState, CommandQueue, drainCommands)
import Server.Network (ClientMap, broadcastGameState)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', readTVar)

-- | Các vị trí chiến thuật định sẵn để sinh ngọc sức mạnh trên bản đồ 32x22
orbSpawnCandidates :: [(Position, OrbType)]
orbSpawnCandidates =
  [ (Pos 15 11, OverchargeOrb)
  , (Pos 16 10, OverchargeOrb)
  , (Pos 10 5,  HealOrb)
  , (Pos 21 5,  HealOrb)
  , (Pos 10 16, EnergyOrb)
  , (Pos 21 16, EnergyOrb)
  , (Pos 4 11,  HealOrb)
  , (Pos 27 11, EnergyOrb)
  , (Pos 15 20, OverchargeOrb)
  , (Pos 16 1,  OverchargeOrb)
  ]

-- | [Chap 5 & Chap 6] Vòng lặp nhịp tim 30 FPS của máy chủ
runTickLoop :: ServerState -> CommandQueue -> ClientMap -> IO ()
runTickLoop stVar cmdQueue clientMap = loop (0 :: Int)
  where
    loop !tickCounter = do
      -- 1. Nhịp tim 30 FPS (~33333 microseconds)
      threadDelay 33333

      -- 2. Thực hiện giao dịch STM nguyên tử
      !latestState <- atomically $ do
        -- Rút toàn bộ lệnh từ hàng đợi TChan
        queuedCmds <- drainCommands cmdQueue

        -- Cập nhật trạng thái game
        modifyTVar' stVar (\(!s0) ->
          let !s1 = foldl' (\(!acc) (!pid, !cmd) -> updateGameState pid cmd acc) s0 queuedCmds
              !s2 = stepGameTick s1
              -- Mỗi 120 ticks (~4s), thử sinh 1 ngọc sức mạnh mới nếu chưa đạt giới hạn
              !s3 = if tickCounter `mod` 120 == 0
                      then
                        let idx = (tickCounter `div` 120) `mod` length orbSpawnCandidates
                            (candidatePos, candidateType) = orbSpawnCandidates !! idx
                            newOrbId = 1000 + (tickCounter `div` 120)
                        in spawnPowerUp newOrbId candidatePos candidateType s2
                      else s2
          in s3
          )

        readTVar stVar

      -- 3. Phát sóng trạng thái mới nhất tới toàn bộ Client
      broadcastGameState clientMap latestState

      -- Lặp tiếp với nhịp tăng dần
      loop (tickCounter + 1)
