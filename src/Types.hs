-- ============================================================================
-- ĐỒ ÁN CUỐI KỲ: MULTIPLAYER TACTICAL 3D HERO TANK BATTLE ARENA (NHÓM 5 THÀNH VIÊN)
-- File: src/Types.hs
-- Phụ trách: Toàn bộ 5 thành viên (Mô hình Dữ liệu Hợp nhất ADT & Chuẩn Giao thức)
-- ============================================================================
-- [Haskell_Chuong_6_ADT_Quy_nap]
-- 1. Sum Types (Kiểu tổng hợp):
--    - Direction: 4 hướng di chuyển và ngắm bắn (North, South, East, West).
--    - TankClass: 5 chiến binh Hero Tank với đặc tính và Tuyệt chiêu Ultimate riêng biệt.
--    - OrbType: 3 loại ngọc năng lượng chiến thuật (HealOrb, EnergyOrb, OverchargeOrb).
--    - Command: Tập lệnh giao thức mở rộng (Move, Shoot, CastUltimate, SendChat, SelectClass, Disconnect).
-- 2. Product Types & Record Syntax (Kiểu tích hợp & Bản ghi):
--    - Position, Tank, Bullet, PowerUp, GameState với các trường dữ liệu tường minh.
-- 3. Strictness Control ({-# LANGUAGE BangPatterns #-}):
--    - Sử dụng tiền tố (!) trên mọi trường bản ghi và tham số, phối hợp Data.Map.Strict
--      để triệt tiêu hoàn toàn thunk ngầm định, chống rò rỉ bộ nhớ (Space Leaks).
-- 4. Typeclass Derivations:
--    - Eq, Show, Read, Enum, Bounded phục vụ so sánh, in ấn chẩn đoán, tuần tự hóa
--      gói tin mạng mà không phụ thuộc thư viện bên ngoài.
-- ============================================================================

{-# LANGUAGE BangPatterns #-}

module Types where

import qualified Data.Map.Strict as Map

-- | Hướng di chuyển và hướng nòng súng của xe tăng / viên đạn
data Direction = North | South | East | West
  deriving (Eq, Show, Read)

-- | Tọa độ trên lưới ô vuông 2D chiến trường (32x22 Grid)
data Position = Pos
  { posX :: !Int
  , posY :: !Int
  } deriving (Eq, Show, Read)

-- | 5 Lớp Chiến Binh Hero Tank với bộ chỉ số và Tuyệt kỹ Ultimate [E] riêng biệt
data TankClass
  = VanguardCyan    -- ^ Tiên phong Băng Giá (Cryo Vanguard): Sóng chấn động băng giá AoE
  | TitanCrimson    -- ^ Thiết giáp Bạo Chúa (Crimson Titan): Pháo kích 5 tên lửa hình nan quạt
  | ViperEmerald    -- ^ Độc xà Hồi phục (Emerald Viper): Hồi phục nano tức thì và khiên độc
  | SolarisGold     -- ^ Quang năng Hủy diệt (Solaris Destroyer): Chùm tia laser xuyên phá toàn bản đồ
  | PhantomViolet   -- ^ Hư không Sát thủ (Phantom Assassin): Dịch chuyển tức thời và bộc phá hư không
  deriving (Eq, Show, Read, Enum, Bounded)

-- | Dữ liệu thực thể Xe tăng của người chơi hoặc Bot AI
data Tank = Tank
  { tankId              :: !Int        -- ^ Mã định danh duy nhất của người chơi / bot
  , tankPos             :: !Position   -- ^ Tọa độ hiện tại trên bàn cờ 32x22
  , tankDir             :: !Direction  -- ^ Hướng nhìn / nòng súng hiện tại
  , tankHealth          :: !Int        -- ^ Lượng máu hiện tại (Tối đa 600 HP)
  , tankEnergy          :: !Int        -- ^ Năng lượng tích lũy tuyệt chiêu (0 đến 100)
  , tankFireCooldown    :: !Int        -- ^ Bộ đếm thời gian hồi bắn (Ticks)
  , tankMoveCooldown    :: !Int        -- ^ Bộ đếm thời gian hồi bước đi (Ticks)
  , tankRespawnTimer    :: !Int        -- ^ Đếm ngược 3 giây hồi sinh khi xe nổ (90 ticks ở 30 FPS)
  , tankInvincibleTimer :: !Int        -- ^ Đếm ngược 2 giây khiên bất tử chớp nháy (60 ticks ở 30 FPS)
  , tankClass           :: !TankClass  -- ^ Hệ chiến binh đang chọn
  } deriving (Eq, Show, Read)

-- | Dữ liệu Viên đạn / Đạn pháo đang bay trên bản đồ
data Bullet = Bullet
  { bulletPos    :: !Position   -- ^ Tọa độ hiện tại của đạn
  , bulletDir    :: !Direction  -- ^ Hướng bay của đạn
  , bulletOwner  :: !Int        -- ^ ID người chơi / bot đã khai hỏa
  , bulletDamage :: !Int        -- ^ Lượng sát thương gây ra khi va chạm (40 thường, 75 rocket, 120 laser)
  } deriving (Eq, Show, Read)

-- | Chủng loại Ngọc sức mạnh (Power-Up Orbs) xuất hiện trên chiến trường
data OrbType
  = HealOrb        -- ^ Hồi phục +150 HP (tối đa 600 HP)
  | EnergyOrb      -- ^ Bơm nạp +40 Năng lượng Ultimate (tối đa 100 Energy)
  | OverchargeOrb  -- ^ Quá tải năng lượng: +30 Điểm, reset hồi chiêu và nạp +50 Energy
  deriving (Eq, Show, Read)

-- | Thực thể Ngọc sức mạnh trôi nổi trên chiến trường 3D
data PowerUp = PowerUp
  { orbId   :: !Int        -- ^ Mã định danh ngọc
  , orbPos  :: !Position   -- ^ Tọa độ ô lưới của ngọc
  , orbType :: !OrbType    -- ^ Phân loại ngọc
  } deriving (Eq, Show, Read)

-- | TRẠNG THÁI TOÀN BỘ THẾ GIỚI GAME (Quản lý tập trung tại Server qua TVar STM)
data GameState = GameState
  { tanks         :: !(Map.Map Int Tank)       -- ^ Danh sách xe tăng đang có trong phòng đấu
  , bullets       :: ![Bullet]                 -- ^ Danh sách các viên đạn đang bay
  , score         :: !(Map.Map Int Int)        -- ^ Bảng điểm số tích lũy của từng người chơi
  , powerUps      :: ![PowerUp]                -- ^ Các ngọc tăng lực đang trôi nổi trên bản đồ
  , chatLog       :: ![(String, String)]       -- ^ Lịch sử hệ thống (tương thích giao thức mạng)
  , gameTickCount :: !Int                      -- ^ Bộ đếm nhịp toàn cục đồng bộ giữa Server và Client
  } deriving (Eq, Show, Read)

-- | Lệnh điều khiển gửi từ Client lên Server qua giao thức mạng TCP
data Command
  = Move !Direction           -- ^ Yêu cầu di chuyển / xoay nòng theo 4 hướng
  | Shoot                     -- ^ Khai hỏa súng đại bác plasma
  | CastUltimate              -- ^ Kích hoạt Tuyệt kỹ Tối Thượng [E] khi đủ 100 Năng lượng
  | SendChat !String          -- ^ Phát ngôn kênh tán gẫu chiến thuật toàn phòng
  | SelectClass !TankClass    -- ^ Chọn lớp tướng xe tăng
  | Disconnect                -- ^ Ngắt kết nối rời trận
  deriving (Eq, Show, Read)

-- ============================================================================
-- CÁC HẰNG SỐ TOÀN CỤC CỦA BẢN ĐỒ CHIẾN TRƯỜNG MỞ RỘNG (32x22 Grid)
-- ============================================================================

-- | Chiều rộng bản đồ lưới (0 đến 31)
gridWidth :: Int
gridWidth = 32

-- | Chiều cao bản đồ lưới (0 đến 21)
gridHeight :: Int
gridHeight = 22

-- | Máu tối đa của xe tăng (Nâng cấp 600 HP tạo trận đấu kịch tính, chiến thuật sâu)
maxHealth :: Int
maxHealth = 600

-- | Năng lượng tối đa của xe tăng
maxEnergy :: Int
maxEnergy = 100

-- | Vị trí các khối tường đá tĩnh (Stone Walls) cản đường xe tăng và triệt tiêu đạn
-- Bố trí chướng ngại vật đối xứng tạo tính chiến thuật sâu cho đấu trường 32x22
walls :: [Position]
walls =
  [ -- 1. Cụm boongke phòng ngự góc Tây Bắc (North-West Fortress)
    Pos 5 17, Pos 5 16, Pos 5 15, Pos 6 17, Pos 7 17
  , Pos 8 14, Pos 9 14, Pos 9 15

    -- 2. Cụm boongke phòng ngự góc Đông Bắc (North-East Fortress)
  , Pos 26 17, Pos 26 16, Pos 26 15, Pos 25 17, Pos 24 17
  , Pos 23 14, Pos 22 14, Pos 22 15

    -- 3. Cụm boongke phòng ngự góc Tây Nam (South-West Fortress)
  , Pos 5 4, Pos 5 5, Pos 5 6, Pos 6 4, Pos 7 4
  , Pos 8 7, Pos 9 7, Pos 9 6

    -- 4. Cụm boongke phòng ngự góc Đông Nam (South-East Fortress)
  , Pos 26 4, Pos 26 5, Pos 26 6, Pos 25 4, Pos 24 4
  , Pos 23 7, Pos 22 7, Pos 22 6

    -- 5. Khối phòng tuyến Trung tâm (Central Tactical Fortresses)
  , Pos 14 10, Pos 14 11, Pos 15 10, Pos 15 11
  , Pos 16 10, Pos 16 11, Pos 17 10, Pos 17 11

    -- 6. Tường che chắn cánh Bắc và cánh Nam (Flanking Corridors)
  , Pos 15 16, Pos 16 16, Pos 15 17, Pos 16 17
  , Pos 15 4,  Pos 16 4,  Pos 15 5,  Pos 16 5

    -- 7. Chốt chặn hành lang phía Tây và phía Đông (Midfield Pillars)
  , Pos 10 10, Pos 10 11, Pos 21 10, Pos 21 11
  ]
