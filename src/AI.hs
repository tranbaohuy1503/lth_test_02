-- ============================================================================
-- ĐỒ ÁN CUỐI KỲ: MULTIPLAYER TACTICAL 3D HERO TANK BATTLE ARENA (NHÓM 5 THÀNH VIÊN)
-- File: src/AI.hs
-- Phụ trách: THÀNH VIÊN 4 (Giám Sát Luồng Chống Rớt Mạng & Bot AI)
-- ============================================================================
-- [Haskell #2 (Sedma Assignment)]
-- 1. Bot State Pattern (BotMemory): Lưu trữ lịch sử, số nhịp suy nghĩ và mục tiêu trước đó.
-- 2. Pure Decision Function (decideBotAction): Hàm toán học thuần túy nhận bộ nhớ hiện tại
--    và GameState để tính toán nước đi tối ưu (Move/Shoot/CastUltimate/Dodge).
-- 3. Tactical AI Behaviors:
--    - Tần số phản xạ chiến thuật ~420ms (tránh spam lệnh, phản xạ tự nhiên).
--    - Né đạn né pháo (Bullet Dodging): Phát hiện đường bay của đạn đối phương và né ngang.
--    - Thu thập ngọc cứu thương: Khi máu < 150 HP, tự động tìm kiếm và nhặt HealOrb.
--    - Kích hoạt Tuyệt Kỹ [E]: Khi tích đủ 100 Năng lượng, lập tức khai hỏa Tuyệt chiêu Ultimate!
-- ============================================================================

{-# LANGUAGE BangPatterns #-}

module AI
  ( BotMemory(..)
  , initBotMemory
  , decideBotAction
  , runBotLoop
  ) where

import Types
import GameLogic (isInsideGrid, isWall, movePos)
import Server.State (ServerState, CommandQueue, addPlayerWithClass, enqueueCommand)
import qualified Data.Map.Strict as Map
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (readTVarIO)
import Data.List (minimumBy, find)
import Data.Ord (comparing)

-- | Cấu trúc bộ nhớ trạng thái nội tại của Bot AI
data BotMemory = BotMemory
  { botId          :: !Int          -- ^ ID định danh của Bot (thường là 999)
  , tickCount      :: !Int          -- ^ Số nhịp suy nghĩ
  , lastTargetId   :: !(Maybe Int)  -- ^ ID đối thủ vừa nhắm tới
  , botChosenClass :: !TankClass    -- ^ Lớp tướng chiến binh của Bot
  } deriving (Eq, Show)

-- | Khởi tạo bộ nhớ ban đầu
initBotMemory :: Int -> BotMemory
initBotMemory !bId = BotMemory
  { botId          = bId
  , tickCount      = 0
  , lastTargetId   = Nothing
  , botChosenClass = TitanCrimson  -- Bot sử dụng Pháo binh Hỏa Thần
  }

-- | [Pure Decision Function] Hàm đưa ra quyết định hành động hoàn toàn thuần túy
decideBotAction :: BotMemory -> GameState -> (Command, BotMemory)
decideBotAction !mem !st =
  let !bId     = botId mem
      !newTick = tickCount mem + 1
      !mem'    = mem { tickCount = newTick }
  in case Map.lookup bId (tanks st) of
       Nothing ->
         -- Nếu xe của Bot chưa có trên bàn cờ, gửi lệnh chọn tướng
         (SelectClass (botChosenClass mem), mem')

       Just botTank ->
         -- Nếu xe đang trong thời gian đếm ngược hồi sinh (3 giây), chờ đợi
         if tankRespawnTimer botTank > 0
           then (Move North, mem')
           else
             let !myPos = tankPos botTank
                 !myDir = tankDir botTank
             in case findThreateningBullet myPos (bullets st) of
                  -- 1. Ưu tiên né đạn nếu có đạn đang bay thẳng vào vị trí
                  Just threatBullet ->
                    let !dodgeDir = pickDodgeDir myPos (bulletDir threatBullet)
                    in (Move dodgeDir, mem')

                  -- Không bị đạn đe dọa trực tiếp
                  Nothing
                    -- 2. Bật tuyệt chiêu nếu đủ 100 năng lượng
                    | tankEnergy botTank >= maxEnergy ->
                        (CastUltimate, mem')

                    -- 3. Tìm ngọc hồi máu khi HP < 300 (Dưới 50% lượng máu tối đa 600 HP)
                    | tankHealth botTank < 300 && hasHealOrb (powerUps st) ->
                        let !nearestHeal = getNearestHealOrb myPos (powerUps st)
                            !stepDir     = navigateToward myPos (orbPos nearestHeal)
                        in (Move stepDir, mem')

                    -- 4. Tấn công hoặc áp sát kẻ địch
                    | otherwise ->
                        let !enemies = filter (\t -> tankId t /= bId && tankRespawnTimer t <= 0)
                                              (Map.elems (tanks st))
                        in if null enemies
                             then patrolMove myPos myDir mem'
                             else
                               let !nearestEnemy = minimumBy (comparing (manhattanDist myPos . tankPos)) enemies
                                   !enemyPos     = tankPos nearestEnemy
                                   !enemyId      = tankId nearestEnemy
                                   !memWithTarg  = mem' { lastTargetId = Just enemyId }
                               in pursueAndEngage myPos myDir enemyPos (tankFireCooldown botTank) memWithTarg

-- | Khoảng cách Manhattan: |x1 - x2| + |y1 - y2|
manhattanDist :: Position -> Position -> Int
manhattanDist (Pos !x1 !y1) (Pos !x2 !y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Phát hiện viên đạn đang bay thẳng vào vị trí xe tăng trong cự ly <= 4 ô
findThreateningBullet :: Position -> [Bullet] -> Maybe Bullet
findThreateningBullet (Pos mx my) bs =
  find isThreatening bs
  where
    isThreatening b = case bulletDir b of
      North -> posX (bulletPos b) == mx && posY (bulletPos b) < my && (my - posY (bulletPos b) <= 4)
      South -> posX (bulletPos b) == mx && posY (bulletPos b) > my && (posY (bulletPos b) - my <= 4)
      East  -> posY (bulletPos b) == my && posX (bulletPos b) < mx && (mx - posX (bulletPos b) <= 4)
      West  -> posY (bulletPos b) == my && posX (bulletPos b) > mx && (posX (bulletPos b) - mx <= 4)

-- | Chọn hướng né đạn vuông góc với quỹ đạo đạn bay
pickDodgeDir :: Position -> Direction -> Direction
pickDodgeDir !p !bDir =
  let candidates = case bDir of
        North -> [East, West]
        South -> [West, East]
        East  -> [North, South]
        West  -> [South, North]
  in pickNonWallDir p candidates

-- | Kiểm tra xem trên bản đồ có ngọc hồi máu không
hasHealOrb :: [PowerUp] -> Bool
hasHealOrb orbs = any (\o -> orbType o == HealOrb) orbs

-- | Lấy ngọc hồi máu gần nhất
getNearestHealOrb :: Position -> [PowerUp] -> PowerUp
getNearestHealOrb !p orbs =
  let healOrbs = filter (\o -> orbType o == HealOrb) orbs
  in minimumBy (comparing (manhattanDist p . orbPos)) healOrbs

-- | Tìm hướng di chuyển tiếp cận một tọa độ đích
navigateToward :: Position -> Position -> Direction
navigateToward !pFrom !pTo =
  let !dx = posX pTo - posX pFrom
      !dy = posY pTo - posY pFrom
      preferred =
        if abs dx > abs dy
          then [if dx > 0 then East else West, if dy > 0 then North else South]
          else [if dy > 0 then North else South, if dx > 0 then East else West]
  in pickNonWallDir pFrom preferred

-- | Chiến thuật giao tranh khi nhắm tới đối thủ
pursueAndEngage :: Position -> Direction -> Position -> Int -> BotMemory -> (Command, BotMemory)
pursueAndEngage !myPos !myDir !ePos !fireCd !mem
  -- CÙNG CỘT DỌC
  | posX myPos == posX ePos =
      if posY ePos > posY myPos
        then -- Địch ở phía Bắc
          if myDir == North && clearPathNorth myPos ePos
            then if fireCd <= 0 then (Shoot, mem) else (Move North, mem)
            else (Move North, mem)
        else -- Địch ở phía Nam
          if myDir == South && clearPathSouth myPos ePos
            then if fireCd <= 0 then (Shoot, mem) else (Move South, mem)
            else (Move South, mem)

  -- CÙNG HÀNG NGANG
  | posY myPos == posY ePos =
      if posX ePos > posX myPos
        then -- Địch ở phía Đông
          if myDir == East && clearPathEast myPos ePos
            then if fireCd <= 0 then (Shoot, mem) else (Move East, mem)
            else (Move East, mem)
        else -- Địch ở phía Tây
          if myDir == West && clearPathWest myPos ePos
            then if fireCd <= 0 then (Shoot, mem) else (Move West, mem)
            else (Move West, mem)

  -- CHƯA THẲNG HÀNG: Tiếp cận theo trục ưu tiên
  | otherwise =
      let !dir = navigateToward myPos ePos
      in (Move dir, mem)

-- | Kiểm tra đường ngắm hướng Bắc không bị tường cản
clearPathNorth :: Position -> Position -> Bool
clearPathNorth (Pos x y1) (Pos _ y2) =
  not (any (\y -> isWall (Pos x y)) [y1 + 1 .. y2 - 1])

-- | Kiểm tra đường ngắm hướng Nam không bị tường cản
clearPathSouth :: Position -> Position -> Bool
clearPathSouth (Pos x y1) (Pos _ y2) =
  not (any (\y -> isWall (Pos x y)) [y2 + 1 .. y1 - 1])

-- | Kiểm tra đường ngắm hướng Đông không bị tường cản
clearPathEast :: Position -> Position -> Bool
clearPathEast (Pos x1 y) (Pos x2 _) =
  not (any (\x -> isWall (Pos x y)) [x1 + 1 .. x2 - 1])

-- | Kiểm tra đường ngắm hướng Tây không bị tường cản
clearPathWest :: Position -> Position -> Bool
clearPathWest (Pos x1 y) (Pos x2 _) =
  not (any (\x -> isWall (Pos x y)) [x2 + 1 .. x1 - 1])

-- | Chọn hướng hợp lệ đầu tiên không bị tường chắn
pickNonWallDir :: Position -> [Direction] -> Direction
pickNonWallDir !p dirs = case filter isSafe dirs of
  (d:_) -> d
  []    -> North
  where
    isSafe d =
      let target = movePos d p
      in isInsideGrid target && not (isWall target)

-- | Tuần tra bản đồ khi không có đối thủ
patrolMove :: Position -> Direction -> BotMemory -> (Command, BotMemory)
patrolMove !pos !dir !mem =
  let !target = movePos dir pos
  in if isInsideGrid target && not (isWall target) && (tickCount mem `mod` 5 /= 0)
       then (Move dir, mem)
       else
         let nextDir = case dir of
               North -> East
               East  -> South
               South -> West
               West  -> North
         in (Move nextDir, mem)

-- | [Chap 5 Green Thread] Vòng lặp điều khiển Bot AI chạy độc lập trên máy chủ
-- Chu kỳ ~420ms giúp bot ra quyết định với nhịp điệu chiến thuật chân thực
runBotLoop :: Int -> CommandQueue -> ServerState -> IO ()
runBotLoop !bId !cmdQueue !stVar = do
  putStrLn $ "[AI Engine] Bot Tank (ID: " ++ show bId ++ ") san sang tham chien!"
  addPlayerWithClass stVar bId TitanCrimson
  loop (initBotMemory bId)
  where
    loop !mem = do
      -- [Chap 5] Nghỉ 420ms (~2.4 hành động / giây) theo nhịp chiến thuật
      threadDelay 420000

      st <- readTVarIO stVar
      let (!cmd, !nextMem) = decideBotAction mem st
      enqueueCommand cmdQueue bId cmd
      loop nextMem
