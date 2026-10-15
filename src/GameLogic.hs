-- ============================================================================
-- ĐỒ ÁN CUỐI KỲ: MULTIPLAYER TACTICAL 3D HERO TANK BATTLE ARENA (NHÓM 5 THÀNH VIÊN)
-- File: src/GameLogic.hs
-- Phụ trách: THÀNH VIÊN 1 (Logic Game & Thiết Kế Dữ Liệu)
-- ============================================================================
-- [Buoi_01_tong_quan_ve_lap_trinh_ham] & [Buoi_03_kieu_du_lieu_va_da_hinh_tham_so]
-- 1. Hàm thuần túy (Pure Functions & Referential Transparency):
--    - Mọi biến đổi trạng thái từ Command, di chuyển, bắn đạn, tuyệt chiêu Ultimate
--      đều là hàm toán học thuần túy f :: State -> Event -> State không gây tác dụng phụ.
-- 2. Higher-Order Functions: Dùng map, filter, foldl', any, find từ Data.List.
-- [LTH_Chuong_7_Evaluation_Functor_Applicative_Monad]
-- 3. Strictness Control & BangPatterns (!): Ép tính toán accumulator về WHNF trong
--    các vòng lặp foldl' xử lý đạn, nhặt ngọc và kiểm tra va chạm, chống tràn bộ nhớ 60 FPS.
-- ============================================================================

{-# LANGUAGE BangPatterns #-}

module GameLogic
  ( movePos
  , isInsideGrid
  , isWall
  , spawnPoints
  , findSafeSpawn
  , spawnTank
  , updateGameState
  , stepGameTick
  , spawnPowerUp
  , collectPowerUps
  ) where

import Types
import qualified Data.Map.Strict as Map
import Data.List (find)

-- | [Buoi 01 - Pure Function] Tính toán tọa độ mới theo hướng di chuyển
-- Tọa độ 2D Cartesian: North là +y, South là -y, East là +x, West là -x
movePos :: Direction -> Position -> Position
movePos dir (Pos !x !y) = case dir of
  North -> Pos x (y + 1)
  South -> Pos x (y - 1)
  East  -> Pos (x + 1) y
  West  -> Pos (x - 1) y

-- | Khoảng cách Chebyshev giữa 2 vị trí ô cờ (max(|dx|, |dy|))
chebyshevDist :: Position -> Position -> Int
chebyshevDist (Pos !x1 !y1) (Pos !x2 !y2) =
  max (abs (x1 - x2)) (abs (y1 - y2))

-- | Khoảng cách Manhattan: |x1 - x2| + |y1 - y2|
manhattanDist :: Position -> Position -> Int
manhattanDist (Pos !x1 !y1) (Pos !x2 !y2) =
  abs (x1 - x2) + abs (y1 - y2)

-- | Kiểm tra tọa độ có nằm gọn trong phạm vi bàn cờ 32x22 không
isInsideGrid :: Position -> Bool
isInsideGrid (Pos !x !y) =
  x >= 0 && x < gridWidth && y >= 0 && y < gridHeight

-- | Kiểm tra tọa độ có đâm vào khối tường đá tĩnh không
isWall :: Position -> Bool
isWall !p = p `elem` walls

-- | Danh sách các tọa độ hồi sinh an toàn xung quanh bản đồ chiến trường 32x22
spawnPoints :: [Position]
spawnPoints =
  [ Pos 2 2,   Pos 29 19, Pos 2 19,  Pos 29 2
  , Pos 15 2,  Pos 16 19, Pos 2 11,  Pos 29 11
  , Pos 6 6,   Pos 25 15, Pos 6 15,  Pos 25 6
  , Pos 12 4,  Pos 19 17, Pos 12 17, Pos 19 4
  ]

-- | [Chap 6 - Pattern Matching & Higher-Order Functions]
-- Tìm vị trí hồi sinh an toàn nhất không bị trùng với tường và xe tăng khác
findSafeSpawn :: [Tank] -> Position
findSafeSpawn aliveTanks =
  let occupiedPos = map tankPos (filter (\t -> tankRespawnTimer t <= 0) aliveTanks)
      isFree p    = not (isWall p) && p `notElem` occupiedPos
  in case find isFree spawnPoints of
       Just safePos -> safePos
       Nothing      -> Pos 1 1 -- Tọa độ dự phòng khẩn cấp

-- | Thêm mới hoặc khởi tạo xe tăng người chơi khi tham gia phòng đấu
spawnTank :: Int -> TankClass -> GameState -> GameState
spawnTank !pId !cls !st =
  let !safeP   = findSafeSpawn (Map.elems (tanks st))
      !newTank = Tank
        { tankId              = pId
        , tankPos             = safeP
        , tankDir             = North
        , tankHealth          = maxHealth
        , tankEnergy          = 0
        , tankFireCooldown    = 0
        , tankMoveCooldown    = 0
        , tankRespawnTimer    = 0
        , tankInvincibleTimer = 60  -- 2 giây khiên bất tử khởi đầu (60 ticks)
        , tankClass           = cls
        }
      !newTanks = Map.insert pId newTank (tanks st)
      !newScore = Map.insertWith (\_ old -> old) pId 0 (score st)
  in st { tanks = newTanks, score = newScore }

-- | Sinh ngọc sức mạnh ngẫu nhiên tại vị trí trống
spawnPowerUp :: Int -> Position -> OrbType -> GameState -> GameState
spawnPowerUp !oId !pos !oType !st =
  if length (powerUps st) >= 6 || isWall pos || not (isInsideGrid pos)
    then st
    else
      let !newOrb = PowerUp { orbId = oId, orbPos = pos, orbType = oType }
      in st { powerUps = newOrb : powerUps st }

-- | Thu thập ngọc sức mạnh khi xe tăng di chuyển vào ô có ngọc
collectPowerUps :: [PowerUp] -> Tank -> ([PowerUp], Tank, Int)
collectPowerUps currentOrbs tank =
  foldl' checkCollect ([], tank, 0) currentOrbs
  where
    checkCollect (!remOrbs, !t, !scoreBonus) orb =
      if orbPos orb == tankPos t
        then
          let !updatedTank = case orbType orb of
                HealOrb ->
                  t { tankHealth = min maxHealth (tankHealth t + 150) }
                EnergyOrb ->
                  t { tankEnergy = min maxEnergy (tankEnergy t + 40) }
                OverchargeOrb ->
                  t { tankEnergy = maxEnergy
                    , tankFireCooldown = 0
                    , tankMoveCooldown = 0
                    }
              !bonus = case orbType orb of
                OverchargeOrb -> 30
                _             -> 15
          in (remOrbs, updatedTank, scoreBonus + bonus)
        else (orb : remOrbs, t, scoreBonus)

-- | [TV1 Core Logic] Cập nhật trạng thái game khi nhận lệnh tức thì từ Client
updateGameState :: Int -> Command -> GameState -> GameState
updateGameState !pId !cmd !st = case cmd of
  -- 1. Lệnh di chuyển hoặc xoay hướng nòng súng
  Move dir ->
    case Map.lookup pId (tanks st) of
      Nothing -> st
      Just tank ->
        -- Nếu xe đang trong trạng thái phát nổ chờ hồi sinh, cấm di chuyển
        if tankRespawnTimer tank > 0
          then st
          else
            let !targetPos = movePos dir (tankPos tank)
                !validGrid  = isInsideGrid targetPos && not (isWall targetPos)
                !collisionOtherTank =
                  any (\t -> tankId t /= pId && tankRespawnTimer t <= 0 && tankPos t == targetPos)
                      (Map.elems (tanks st))

                -- Chỉ cho phép bước đi nếu hồi chiêu bước đi <= 0
                !canStep = tankMoveCooldown tank <= 0 && validGrid && not collisionOtherTank

                !movedTank =
                  if canStep
                    then tank { tankPos = targetPos, tankDir = dir, tankMoveCooldown = 2 }
                    else tank { tankDir = dir } -- Xoay hướng nòng súng tại chỗ

                -- Thu thập ngọc trên đường đi
                (!remOrbs, !finalTank, !orbBonusScore) =
                  collectPowerUps (powerUps st) movedTank

                !newTanks  = Map.insert pId finalTank (tanks st)
                !newScores = if orbBonusScore > 0
                               then Map.insertWith (+) pId orbBonusScore (score st)
                                else score st
            in st { tanks = newTanks, powerUps = remOrbs, score = newScores }

  -- 2. Khai hỏa súng đại bác plasma
  Shoot ->
    case Map.lookup pId (tanks st) of
      Nothing -> st
      Just tank ->
        if tankRespawnTimer tank > 0 || tankFireCooldown tank > 0
          then st
          else
            let !barrelPos = movePos (tankDir tank) (tankPos tank)
                !cd        = case tankClass tank of
                               TitanCrimson -> 12 -- Pháo hạng nặng bắn chậm hơn chút
                               PhantomViolet -> 7  -- Sát thủ ra đòn nhanh
                               _            -> 9  -- Tiêu chuẩn ~0.3s
                !tankAfterShoot = tank
                  { tankFireCooldown = cd
                  , tankEnergy       = min maxEnergy (tankEnergy tank + 3)
                  }
                !newTanks = Map.insert pId tankAfterShoot (tanks st)
            in if isInsideGrid barrelPos && not (isWall barrelPos)
                 then
                   let !newBullet = Bullet
                         { bulletPos    = barrelPos
                         , bulletDir    = tankDir tank
                         , bulletOwner  = pId
                         , bulletDamage = 40
                         }
                   in st { tanks = newTanks, bullets = newBullet : bullets st }
                 else st { tanks = newTanks }

  -- 3. Kích hoạt Tuyệt Kỹ Tối Thượng Ultimate [E]
  CastUltimate ->
    case Map.lookup pId (tanks st) of
      Nothing -> st
      Just tank ->
        if tankRespawnTimer tank > 0 || tankEnergy tank < maxEnergy
          then st
          else
            let !tankAfterUlt = tank { tankEnergy = 0 }
            in executeHeroUltimate pId (tankClass tank) tankAfterUlt st

  -- 4. Chọn Lớp Tướng Xe Tăng (Hero Selection)
  SelectClass newCls ->
    case Map.lookup pId (tanks st) of
      Nothing ->
        -- Nếu chưa tồn tại, khởi tạo xe tăng với lớp tướng đã chọn
        spawnTank pId newCls st
      Just tank ->
        let !updatedTank = tank { tankClass = newCls }
        in st { tanks = Map.insert pId updatedTank (tanks st) }

  -- 5. Lệnh tán gẫu (Đã lược bỏ để giao diện thông thoáng, tương thích giao thức)
  SendChat _ -> st

  -- 6. Ngắt kết nối rời trận
  Disconnect ->
    let !newTanks = Map.delete pId (tanks st)
    in st { tanks = newTanks }

-- | [Hero Ultimate Execution] Triển khai kỹ năng tối thượng cho 5 hệ chiến binh
executeHeroUltimate :: Int -> TankClass -> Tank -> GameState -> GameState
executeHeroUltimate !pId !cls !tank !st = case cls of
  -- 1. VANGUARD CYAN: "Chấn Động Băng Giá" (Cryo Nova)
  -- Sóng xung kích AoE 5x5 gây 65 sát thương cho mọi kẻ địch xung quanh và làm tê liệt năng lượng
  VanguardCyan ->
    let !myPos = tankPos tank
        !affectedTanks = Map.map (\t ->
          if tankId t /= pId && tankRespawnTimer t <= 0 && chebyshevDist myPos (tankPos t) <= 2
            then
              let !dmg = 85
                  !h'  = tankHealth t - dmg
              in if h' <= 0
                   then t { tankHealth = 0, tankRespawnTimer = 90 }
                   else t { tankHealth = h', tankEnergy = 0, tankFireCooldown = 15 }
            else t
          ) (tanks st)
        !updatedTanks = Map.insert pId tank affectedTanks
    in st { tanks = updatedTanks }

  -- 2. TITAN CRIMSON: "Pháo Kích Hỏa Thần" (Titan Barrage)
  -- Bắn chùm 5 tên lửa hạng nặng (sát thương 75 mỗi quả) càn quét về phía trước
  TitanCrimson ->
    let !baseDir   = tankDir tank
        !barrelPos = movePos baseDir (tankPos tank)
        !rockets   =
          [ Bullet
              { bulletPos    = barrelPos
              , bulletDir    = d
              , bulletOwner  = pId
              , bulletDamage = 75
              }
          | d <- [baseDir, baseDir, rotateCW baseDir, rotateCCW baseDir, baseDir]
          ]
        !newTanks = Map.insert pId tank (tanks st)
    in st { tanks = newTanks, bullets = rockets ++ bullets st }

  -- 3. VIPER EMERALD: "Khiên Độc Phục Hồi" (Nano Poison Shield)
  -- Hồi phục tức thì +200 HP, bật 3 giây khiên bảo vệ và phát tán độc tố 60 sát thương cho đối thủ tiếp xúc
  ViperEmerald ->
    let !myPos = tankPos tank
        !healedTank = tank
          { tankHealth          = min maxHealth (tankHealth tank + 200)
          , tankInvincibleTimer = 90 -- 3 giây bất tử
          }
        !poisonedTanks = Map.map (\t ->
          if tankId t /= pId && tankRespawnTimer t <= 0 && manhattanDist myPos (tankPos t) <= 2
            then
              let !h' = tankHealth t - 60
              in if h' <= 0
                   then t { tankHealth = 0, tankRespawnTimer = 90 }
                   else t { tankHealth = h' }
            else t
          ) (tanks st)
        !updatedTanks = Map.insert pId healedTank poisonedTanks
    in st { tanks = updatedTanks }

  -- 4. SOLARIS GOLD: "Chùm Tia Hyperion" (Hyperion Piercing Beam)
  -- Bắn chùm tia laze hủy diệt xuyên thấu toàn bộ vật cản và xe tăng trên một đường thẳng
  SolarisGold ->
    let !dir = tankDir tank
        !beamPositions = take 25 (iterate (movePos dir) (movePos dir (tankPos tank)))
        !validPositions = filter isInsideGrid beamPositions
        !beamBullets =
          [ Bullet
              { bulletPos    = p
              , bulletDir    = dir
              , bulletOwner  = pId
              , bulletDamage = 120
              }
          | p <- validPositions
          ]
        !newTanks = Map.insert pId tank (tanks st)
    in st { tanks = newTanks, bullets = beamBullets ++ bullets st }

  -- 5. PHANTOM VIOLET: "Dịch Chuyển Hư Không" (Void Blink)
  -- Dịch chuyển 4 ô về phía trước xuyên qua tường và phát nổ 80 sát thương tại điểm đến
  PhantomViolet ->
    let !dir = tankDir tank
        !targetP4 = iterate (movePos dir) (tankPos tank) !! 4
        !destPos  = if isInsideGrid targetP4 && not (isWall targetP4)
                      then targetP4
                      else tankPos tank
        !teleportedTank = tank { tankPos = destPos }
        !voidBlastTanks = Map.map (\t ->
          if tankId t /= pId && tankRespawnTimer t <= 0 && chebyshevDist destPos (tankPos t) <= 2
            then
              let !h' = tankHealth t - 80
              in if h' <= 0
                   then t { tankHealth = 0, tankRespawnTimer = 90 }
                   else t { tankHealth = h' }
            else t
          ) (tanks st)
        !updatedTanks = Map.insert pId teleportedTank voidBlastTanks
    in st { tanks = updatedTanks }

-- | Xoay góc 90 độ theo chiều kim đồng hồ
rotateCW :: Direction -> Direction
rotateCW North = East
rotateCW East  = South
rotateCW South = West
rotateCW West  = North

-- | Xoay góc 90 độ ngược chiều kim đồng hồ
rotateCCW :: Direction -> Direction
rotateCCW North = West
rotateCCW West  = South
rotateCCW South = East
rotateCCW East  = North

-- | [TV1 Core Tick] Xử lý nhịp thế giới 30 FPS:
-- 1. Giảm hồi chiêu và tích lũy năng lượng tự nhiên cực chậm (+1 điểm mỗi 45 ticks)
-- 2. Đếm ngược 3 giây hồi sinh xác xe cháy (90 ticks) và 2 giây khiên bất tử (60 ticks)
-- 3. Cho đạn bay và xử lý va chạm sát thương kèm tích nạp Năng Lượng khi bắn trúng
stepGameTick :: GameState -> GameState
stepGameTick !st =
  let !currentTick = gameTickCount st + 1

      -- 1. Cập nhật chỉ số nội tại của từng xe tăng
      !updatedTanks = Map.map (stepSingleTank currentTick) (tanks st)

      -- 2. Cho tất cả viên đạn bay 1 ô theo hướng của chúng
      !movedBullets = [ b { bulletPos = movePos (bulletDir b) (bulletPos b) }
                      | b <- bullets st
                      ]

      -- 3. Lọc bỏ đạn bay ra khỏi bản đồ hoặc đâm vào tường đá
      !activeBullets = filter (\b -> isInsideGrid (bulletPos b) && not (isWall (bulletPos b)))
                              movedBullets

      -- 4. Xử lý va chạm đạn với các xe tăng
      (!survivingBullets, !tanksAfterCombat, !scoresAfterCombat) =
        resolveBulletCollisions activeBullets updatedTanks (score st)
  in st
       { tanks         = tanksAfterCombat
       , bullets       = survivingBullets
       , score         = scoresAfterCombat
       , gameTickCount = currentTick
       }
  where
    stepSingleTank !tick !t =
      let !fireCd'   = max 0 (tankFireCooldown t - 1)
          !moveCd'   = max 0 (tankMoveCooldown t - 1)
          !invinc'   = max 0 (tankInvincibleTimer t - 1)
          -- Hồi năng lượng/thịnh nộ tự nhiên cực chậm (+1 điểm mỗi 45 ticks ~ 1.5s)
          -- Năng lượng chủ yếu tích lũy khi bắn trúng kẻ thù (+18 điểm khi trúng, +35 điểm khi hạ gục)
          !energy'   = if tankRespawnTimer t <= 0 && tankEnergy t < maxEnergy
                         then if tick `mod` 45 == 0
                                then min maxEnergy (tankEnergy t + 1)
                                else tankEnergy t
                         else tankEnergy t

          -- Xử lý bộ đếm hồi sinh 3 giây
          !tAfterRespawn =
            if tankRespawnTimer t > 1
              then t { tankRespawnTimer = tankRespawnTimer t - 1 }
              else if tankRespawnTimer t == 1
                then
                  -- [Hồi Sinh Hoàn Tất]: Về vị trí an toàn, phục hồi 600 HP và bật khiên 2 giây
                  let !safePos = findSafeSpawn (Map.elems (tanks st))
                  in t { tankHealth          = maxHealth
                       , tankPos             = safePos
                       , tankEnergy          = 0
                       , tankRespawnTimer    = 0
                       , tankInvincibleTimer = 60
                       }
                else t
      in tAfterRespawn
           { tankFireCooldown    = fireCd'
           , tankMoveCooldown    = moveCd'
           , tankInvincibleTimer = invinc'
           , tankEnergy          = energy'
           }

-- | [Chap 1 & Chap 7 - Pure Fold & Strict Accumulators]
-- Duyệt từng viên đạn và kiểm tra va chạm với xe tăng đối thủ
resolveBulletCollisions
  :: [Bullet]
  -> Map.Map Int Tank
  -> Map.Map Int Int
  -> ([Bullet], Map.Map Int Tank, Map.Map Int Int)
resolveBulletCollisions rawBullets initialTanks initialScores =
  foldl' processOneBullet ([], initialTanks, initialScores) rawBullets
  where
    processOneBullet (!bAcc, !tAcc, !sAcc) !b =
      -- Tìm xe tăng đối thủ đứng cùng tọa độ với viên đạn
      let mVictim = find (\t -> tankId t /= bulletOwner b
                             && tankPos t == bulletPos b
                             && tankRespawnTimer t <= 0) -- Không bắn vào xác xe đang đợi hồi sinh
                         (Map.elems tAcc)
      in case mVictim of
           Nothing ->
             -- Không va chạm: Đạn tiếp tục bay
             (b : bAcc, tAcc, sAcc)

           Just victim ->
             -- Nếu xe đang bật Khiên Bất Tử (Invincible Shield), đạn bị chặn đứng không gây sát thương
             if tankInvincibleTimer victim > 0
               then (bAcc, tAcc, sAcc)
               else
                 let !dmg        = bulletDamage b
                     !newHealth  = tankHealth victim - dmg
                     !isKilled   = newHealth <= 0
                     !scoreBonus = if isKilled then 60 else 15

                     !tanksAfterHit =
                       if isKilled
                         then
                           -- Xe tăng bị hạ gục: Máu về 0, kích hoạt bộ đếm 3 giây hồi sinh xác xe cháy (90 ticks)
                           let !wreckTank = victim
                                 { tankHealth       = 0
                                 , tankRespawnTimer = 90
                                 }
                           in Map.insert (tankId victim) wreckTank tAcc
                         else
                           -- Trừ máu bình thường
                           Map.insert (tankId victim) (victim { tankHealth = newHealth }) tAcc

                     -- TÍCH NẠP NĂNG LƯỢNG / THỊNH NỘ CHO NGƯỜI BẮN KHI BẮN TRÚNG MỤC TIÊU (+18 khi trúng, +35 khi hạ gục)
                     !tanksWithShooterEnergy =
                       case Map.lookup (bulletOwner b) tanksAfterHit of
                         Just shooter ->
                           let !bonusEnergy = if isKilled then 35 else 18
                               !newShooter  = shooter { tankEnergy = min maxEnergy (tankEnergy shooter + bonusEnergy) }
                           in Map.insert (bulletOwner b) newShooter tanksAfterHit
                         Nothing -> tanksAfterHit

                     -- Cộng điểm thưởng cho người bắn
                     !scoresAfterHit = Map.insertWith (+) (bulletOwner b) scoreBonus sAcc
                 in (bAcc, tanksWithShooterEnergy, scoresAfterHit)
