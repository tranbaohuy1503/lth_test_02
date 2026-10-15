-- ============================================================================
-- ĐỒ ÁN CUỐI KỲ: MULTIPLAYER TACTICAL 3D HERO TANK BATTLE ARENA (NHÓM 5 THÀNH VIÊN)
-- File: src/Client/UI.hs
-- Phụ trách: THÀNH VIÊN 5 (Lead Game UI/UX & Functional Graphics Engineer)
-- ============================================================================
-- [Chương 6] ADT & Smart Constructor:
-- 1. AppScreen, InputField phân loại trạng thái màn hình và trường nhập liệu.
-- 2. mkCallsign dùng Monad Either String để xác thực tên phi công.
-- [Chương 7] Strictness Control & BangPatterns (!):
-- 3. Ép đánh giá nghiêm ngặt toàn bộ hệ thống hạt (Particle System), sóng xung kích (Shockwave)
--    và phép nội suy chuyển động (Lerp) ở 60 FPS, ngăn chặn triệt để Space Leaks.
-- [Gloss 2.5D Extrusion Engine & Pure Functional Vietnamese Typography]:
-- 4. Kỹ thuật đùn đa tầng (Multi-layer Extrusion) giả lập chiều sâu 3D bằng bóng đổ,
--    mặt vát cạnh đứng, trần nổi phản quang và quầng sáng neon.
-- 5. BỘ KẾT XUẤT CHỮ TIẾNG VIỆT VECTOR THUẦN TÚY (drawVietnameseText):
--    Phân tích từng ký tự tiếng Việt thành ký tự Latinh gốc + vector dấu thanh
--    (Sắc, Huyền, Hỏi, Ngã, Nặng, Mũ, Móc, Trăng, Đ), vẽ đa tầng chống nhòe nét,
--    hiển thị 100% đầy đủ dấu tiếng Việt chuẩn xác trên mọi phiên bản Gloss GLUT!
-- ============================================================================

{-# LANGUAGE BangPatterns #-}

module Client.UI
  ( AppScreen(..)
  , InputField(..)
  , Particle(..)
  , Shockwave(..)
  , mkCallsign
  , nextClass
  , prevClass
  , heroClassInfo
  , mockGameState
  , drawLoginScreen
  , draw3DPlayingScene
  , drawHeroTank3D
  , drawVietnameseText
  , toScreenPos
  , cellSize
  , targetAngle
  ) where

import Types
import Graphics.Gloss
import qualified Data.Map.Strict as Map
import Data.Char (isAlphaNum, toUpper)

-- ============================================================================
-- CÁC HẰNG SỐ KÍCH THƯỚC BẢN ĐỒ & KHUNG NHÌN CHUẨN (1280x720 VIEWPORT)
-- ============================================================================

-- | Kích thước mỗi ô cờ (pixels) chuẩn hóa để 32x22 nằm gọn tuyệt đối trong màn hình
cellSize :: Float
cellSize = 26.0

-- | Nửa chiều rộng và chiều cao sân đấu tính theo pixel (32 * 26 = 832, 22 * 26 = 572)
arenaHalfW :: Float
arenaHalfW = fromIntegral gridWidth * cellSize / 2.0   -- 416.0

arenaHalfH :: Float
arenaHalfH = fromIntegral gridHeight * cellSize / 2.0  -- 286.0

-- | Dịch chuyển tọa độ ô lưới (0..31, 0..21) sang tọa độ màn hình Gloss (-416..416, -286..286)
toScreenPos :: Position -> (Float, Float)
toScreenPos (Pos !x !y) =
  ( fromIntegral x * cellSize - arenaHalfW + cellSize / 2.0
  , fromIntegral y * cellSize - arenaHalfH + cellSize / 2.0
  )

-- ============================================================================
-- MÔ HÌNH HÓA TRẠNG THÁI GIAO DIỆN (ALGEBRAIC DATA TYPES)
-- ============================================================================

data AppScreen
  = LoginScreen
  | ConnectingScreen !String
  | PlayingScreen
  deriving (Eq, Show)

data InputField
  = FieldCallsign
  | FieldServerHost
  | FieldServerPort
  deriving (Eq, Show)

nextClass :: TankClass -> TankClass
nextClass PhantomViolet = VanguardCyan
nextClass c             = succ c

prevClass :: TankClass -> TankClass
prevClass VanguardCyan = PhantomViolet
prevClass c            = pred c

mkCallsign :: String -> Either String String
mkCallsign raw =
  let trimmed = filter (\c -> c /= ' ' && isAlphaNum c) raw
  in if null trimmed
       then Left "Ten phi cong khong duoc de trong!"
       else if length trimmed > 12
         then Left "Toi da 12 ky tu chu hoac so!"
         else Right (map toUpper trimmed)

-- ============================================================================
-- BỘ DỮ LIỆU 5 CHIẾN BINH HERO TANK (ĐẦY ĐỦ TIẾNG VIỆT CÓ DẤU)
-- ============================================================================

data HeroInfo = HeroInfo
  { heroTitle     :: !String
  , heroRole      :: !String
  , heroUltName   :: !String
  , heroUltDesc1  :: !String
  , heroUltDesc2  :: !String
  , heroStatHp    :: !Float
  , heroStatSpd   :: !Float
  , heroStatAtk   :: !Float
  , heroStatDef   :: !Float
  , heroPrimaryCol:: !Color
  , heroAccentCol :: !Color
  , heroGlowCol   :: !Color
  }

heroClassInfo :: TankClass -> HeroInfo
heroClassInfo cls = case cls of
  VanguardCyan -> HeroInfo
    { heroTitle      = "Tiên Phong Băng Giá"
    , heroRole       = "Vai trò: Đấu sĩ - Khống chế diện rộng"
    , heroUltName    = "CHẤN ĐỘNG BĂNG GIÁ"
    , heroUltDesc1   = "Phóng sóng xung kích cực đại 5x5 gây 65 sát thương"
    , heroUltDesc2   = "và đóng băng năng lượng của toàn bộ kẻ địch xung quanh."
    , heroStatHp     = 0.85
    , heroStatSpd    = 0.80
    , heroStatAtk    = 0.70
    , heroStatDef    = 0.80
    , heroPrimaryCol = makeColorI 15 150 215 255
    , heroAccentCol  = makeColorI 170 240 255 255
    , heroGlowCol    = makeColorI 0 230 255 200
    }
  TitanCrimson -> HeroInfo
    { heroTitle      = "Thiết Giáp Bạo Chúa"
    , heroRole       = "Vai trò: Pháo thủ hạng nặng - Càn quét"
    , heroUltName    = "PHÁO KÍCH HỎA THẦN"
    , heroUltDesc1   = "Khai hỏa chùm 5 hỏa tiễn hạng nặng càn quét phía trước,"
    , heroUltDesc2   = "gây 70 sát thương cực lớn cho mỗi đầu đạn phát nổ."
    , heroStatHp     = 1.00
    , heroStatSpd    = 0.55
    , heroStatAtk    = 0.95
    , heroStatDef    = 0.90
    , heroPrimaryCol = makeColorI 200 35 45 255
    , heroAccentCol  = makeColorI 255 160 160 255
    , heroGlowCol    = makeColorI 255 45 45 200
    }
  ViperEmerald -> HeroInfo
    { heroTitle      = "Độc Xà Hồi Phục"
    , heroRole       = "Vai trò: Hỗ trợ sinh tồn - Độc tính"
    , heroUltName    = "KHIÊN ĐỘC PHỤC HỒI"
    , heroUltDesc1   = "Hồi phục tức thì +120 Máu, bật khiên bất tử 3 giây"
    , heroUltDesc2   = "và phát tán độc khí gây 45 sát thương lên đối thủ gần kề."
    , heroStatHp     = 0.75
    , heroStatSpd    = 0.90
    , heroStatAtk    = 0.65
    , heroStatDef    = 0.85
    , heroPrimaryCol = makeColorI 25 165 75 255
    , heroAccentCol  = makeColorI 180 255 190 255
    , heroGlowCol    = makeColorI 40 255 100 200
    }
  SolarisGold -> HeroInfo
    { heroTitle      = "Quang Năng Hủy Diệt"
    , heroRole       = "Vai trò: Xạ thủ bắn tỉa - Laze xuyên thấu"
    , heroUltName    = "CHÙM TIA HYPERION"
    , heroUltDesc1   = "Bắn chùm tia laze hủy diệt xuyên thấu toàn bộ vật cản,"
    , heroUltDesc2   = "thiêu rụi mọi xe tăng trên đường ngắm với 110 sát thương."
    , heroStatHp     = 0.70
    , heroStatSpd    = 0.75
    , heroStatAtk    = 1.00
    , heroStatDef    = 0.65
    , heroPrimaryCol = makeColorI 215 155 15 255
    , heroAccentCol  = makeColorI 255 240 165 255
    , heroGlowCol    = makeColorI 255 210 20 200
    }
  PhantomViolet -> HeroInfo
    { heroTitle      = "Hư Không Sát Thủ"
    , heroRole       = "Vai trò: Sát thủ đột kích - Biến ảnh"
    , heroUltName    = "DỊCH CHUYỂN HƯ KHÔNG"
    , heroUltDesc1   = "Dịch chuyển xuyên tường 4 ô thẳng về phía trước"
    , heroUltDesc2   = "và kích nổ sóng xung kích hư không gây 55 sát thương điểm đến."
    , heroStatHp     = 0.65
    , heroStatSpd    = 1.00
    , heroStatAtk    = 0.85
    , heroStatDef    = 0.60
    , heroPrimaryCol = makeColorI 145 45 205 255
    , heroAccentCol  = makeColorI 230 180 255 255
    , heroGlowCol    = makeColorI 190 55 255 200
    }

-- ============================================================================
-- HỆ THỐNG HIỆU ỨNG HẠT VÀ SÓNG NỔ CLIENT-SIDE (PARTICLE SYSTEM)
-- ============================================================================

data Particle = Particle
  { partX     :: !Float
  , partY     :: !Float
  , partVX    :: !Float
  , partVY    :: !Float
  , partLife  :: !Float
  , partDecay :: !Float
  , partSize  :: !Float
  , partColor :: !Color
  } deriving (Eq, Show)

data Shockwave = Shockwave
  { shockX      :: !Float
  , shockY      :: !Float
  , shockRadius :: !Float
  , shockMaxR   :: !Float
  , shockLife   :: !Float
  , shockColor  :: !Color
  } deriving (Eq, Show)

-- ============================================================================
-- BỘ KẾT XUẤT CHỮ VECTOR TIẾNG VIỆT THUẦN TÚY (PURE FUNCTIONAL DIACRITIC ENGINE)
-- ============================================================================

data Tone = TNothing | TSac | THuyen | THoi | TNga | TNang deriving (Eq, Show)
data VMod = VNothing | VMu | VTrang | VMoc | VBarD deriving (Eq, Show)

data VnChar = VnChar
  { vcBase :: !Char
  , vcMod  :: !VMod
  , vcTone :: !Tone
  } deriving (Eq, Show)

-- | Ánh xạ toàn bộ ký tự Unicode tiếng Việt sang dạng cơ sở + dấu
parseVnChar :: Char -> VnChar
parseVnChar c = case c of
  -- A & Ă & Â
  'á' -> VnChar 'a' VNothing TSac; 'à' -> VnChar 'a' VNothing THuyen; 'ả' -> VnChar 'a' VNothing THoi
  'ã' -> VnChar 'a' VNothing TNga; 'ạ' -> VnChar 'a' VNothing TNang
  'ă' -> VnChar 'a' VTrang TNothing; 'ắ' -> VnChar 'a' VTrang TSac; 'ằ' -> VnChar 'a' VTrang THuyen
  'ẳ' -> VnChar 'a' VTrang THoi;   'ẵ' -> VnChar 'a' VTrang TNga; 'ặ' -> VnChar 'a' VTrang TNang
  'â' -> VnChar 'a' VMu TNothing;   'ấ' -> VnChar 'a' VMu TSac;   'ầ' -> VnChar 'a' VMu THuyen
  'ẩ' -> VnChar 'a' VMu THoi;     'ẫ' -> VnChar 'a' VMu TNga;   'ậ' -> VnChar 'a' VMu TNang
  'Á' -> VnChar 'A' VNothing TSac; 'À' -> VnChar 'A' VNothing THuyen; 'Ả' -> VnChar 'A' VNothing THoi
  'Ã' -> VnChar 'A' VNothing TNga; 'Ạ' -> VnChar 'A' VNothing TNang
  'Ă' -> VnChar 'A' VTrang TNothing; 'Ắ' -> VnChar 'A' VTrang TSac; 'Ằ' -> VnChar 'A' VTrang THuyen
  'Ẳ' -> VnChar 'A' VTrang THoi;   'Ẵ' -> VnChar 'A' VTrang TNga; 'Ặ' -> VnChar 'A' VTrang TNang
  'Â' -> VnChar 'A' VMu TNothing;   'Ấ' -> VnChar 'A' VMu TSac;   'Ầ' -> VnChar 'A' VMu THuyen
  'Ẩ' -> VnChar 'A' VMu THoi;     'Ẫ' -> VnChar 'A' VMu TNga;   'Ậ' -> VnChar 'A' VMu TNang

  -- E & Ê
  'é' -> VnChar 'e' VNothing TSac; 'è' -> VnChar 'e' VNothing THuyen; 'ẻ' -> VnChar 'e' VNothing THoi
  'ẽ' -> VnChar 'e' VNothing TNga; 'ẹ' -> VnChar 'e' VNothing TNang
  'ê' -> VnChar 'e' VMu TNothing;   'ế' -> VnChar 'e' VMu TSac;   'ề' -> VnChar 'e' VMu THuyen
  'ể' -> VnChar 'e' VMu THoi;     'ễ' -> VnChar 'e' VMu TNga;   'ệ' -> VnChar 'e' VMu TNang
  'É' -> VnChar 'E' VNothing TSac; 'È' -> VnChar 'E' VNothing THuyen; 'Ẻ' -> VnChar 'E' VNothing THoi
  'Ẽ' -> VnChar 'E' VNothing TNga; 'Ẹ' -> VnChar 'E' VNothing TNang
  'Ê' -> VnChar 'E' VMu TNothing;   'Ế' -> VnChar 'E' VMu TSac;   'Ề' -> VnChar 'E' VMu THuyen
  'Ể' -> VnChar 'E' VMu THoi;     'Ễ' -> VnChar 'E' VMu TNga;   'Ệ' -> VnChar 'E' VMu TNang

  -- I
  'í' -> VnChar 'i' VNothing TSac; 'ì' -> VnChar 'i' VNothing THuyen; 'ỉ' -> VnChar 'i' VNothing THoi
  'ĩ' -> VnChar 'i' VNothing TNga; 'ị' -> VnChar 'i' VNothing TNang
  'Í' -> VnChar 'I' VNothing TSac; 'Ì' -> VnChar 'I' VNothing THuyen; 'Ỉ' -> VnChar 'I' VNothing THoi
  'Ĩ' -> VnChar 'I' VNothing TNga; 'Ị' -> VnChar 'I' VNothing TNang

  -- O & Ô & Ơ
  'ó' -> VnChar 'o' VNothing TSac; 'ò' -> VnChar 'o' VNothing THuyen; 'ỏ' -> VnChar 'o' VNothing THoi
  'õ' -> VnChar 'o' VNothing TNga; 'ọ' -> VnChar 'o' VNothing TNang
  'ô' -> VnChar 'o' VMu TNothing;   'ố' -> VnChar 'o' VMu TSac;   'ồ' -> VnChar 'o' VMu THuyen
  'ổ' -> VnChar 'o' VMu THoi;     'ỗ' -> VnChar 'o' VMu TNga;   'ộ' -> VnChar 'o' VMu TNang
  'ơ' -> VnChar 'o' VMoc TNothing;  'ớ' -> VnChar 'o' VMoc TSac;  'ờ' -> VnChar 'o' VMoc THuyen
  'ở' -> VnChar 'o' VMoc THoi;    'ỡ' -> VnChar 'o' VMoc TNga;  'ợ' -> VnChar 'o' VMoc TNang
  'Ó' -> VnChar 'O' VNothing TSac; 'Ò' -> VnChar 'O' VNothing THuyen; 'Ỏ' -> VnChar 'O' VNothing THoi
  'Õ' -> VnChar 'O' VNothing TNga; 'Ọ' -> VnChar 'O' VNothing TNang
  'Ô' -> VnChar 'O' VMu TNothing;   'Ố' -> VnChar 'O' VMu TSac;   'Ồ' -> VnChar 'O' VMu THuyen
  'Ổ' -> VnChar 'O' VMu THoi;     'Ỗ' -> VnChar 'O' VMu TNga;   'Ộ' -> VnChar 'O' VMu TNang
  'Ơ' -> VnChar 'O' VMoc TNothing;  'Ớ' -> VnChar 'O' VMoc TSac;  'Ờ' -> VnChar 'O' VMoc THuyen
  'Ở' -> VnChar 'O' VMoc THoi;    'Ỡ' -> VnChar 'O' VMoc TNga;  'Ợ' -> VnChar 'O' VMoc TNang

  -- U & Ư
  'ú' -> VnChar 'u' VNothing TSac; 'ù' -> VnChar 'u' VNothing THuyen; 'ủ' -> VnChar 'u' VNothing THoi
  'ũ' -> VnChar 'u' VNothing TNga; 'ụ' -> VnChar 'u' VNothing TNang
  'ư' -> VnChar 'u' VMoc TNothing;  'ứ' -> VnChar 'u' VMoc TSac;  'ừ' -> VnChar 'u' VMoc THuyen
  'ử' -> VnChar 'u' VMoc THoi;    'ữ' -> VnChar 'u' VMoc TNga;  'ự' -> VnChar 'u' VMoc TNang
  'Ú' -> VnChar 'U' VNothing TSac; 'Ù' -> VnChar 'U' VNothing THuyen; 'Ủ' -> VnChar 'U' VNothing THoi
  'Ũ' -> VnChar 'U' VNothing TNga; 'Ụ' -> VnChar 'U' VNothing TNang
  'Ư' -> VnChar 'U' VMoc TNothing;  'Ứ' -> VnChar 'U' VMoc TSac;  'Ừ' -> VnChar 'U' VMoc THuyen
  'Ử' -> VnChar 'U' VMoc THoi;    'Ữ' -> VnChar 'U' VMoc TNga;  'Ự' -> VnChar 'U' VMoc TNang

  -- Y
  'ý' -> VnChar 'y' VNothing TSac; 'ỳ' -> VnChar 'y' VNothing THuyen; 'ỷ' -> VnChar 'y' VNothing THoi
  'ỹ' -> VnChar 'y' VNothing TNga; 'ỵ' -> VnChar 'y' VNothing TNang
  'Ý' -> VnChar 'Y' VNothing TSac; 'Ỳ' -> VnChar 'Y' VNothing THuyen; 'Ỷ' -> VnChar 'Y' VNothing THoi
  'Ỹ' -> VnChar 'Y' VNothing TNga; 'Ỵ' -> VnChar 'Y' VNothing TNang

  -- Đ / đ
  'đ' -> VnChar 'd' VBarD TNothing
  'Đ' -> VnChar 'D' VBarD TNothing

  -- Các ký tự thường
  other -> VnChar other VNothing TNothing

-- | Độ rộng tương đối của một ký tự trong GLUT Stroke Roman Font
charWidth :: Char -> Float
charWidth c = case c of
  'i' -> 36.0; 'j' -> 40.0; 'l' -> 36.0; 'I' -> 42.0; '1' -> 48.0
  'm' -> 118.0; 'w' -> 110.0; 'M' -> 124.0; 'W' -> 124.0
  ' ' -> 55.0; '.' -> 32.0; ':' -> 35.0; '-' -> 50.0; '[' -> 45.0; ']' -> 45.0
  ch | ch >= 'A' && ch <= 'Z' -> 94.0
     | otherwise              -> 76.0

-- | Vẽ một chuỗi văn bản tiếng Việt hoàn chỉnh có đầy đủ dấu sắc sảo
drawVietnameseText :: Float -> Color -> String -> Picture
drawVietnameseText !txtScale !txtColor !str =
  scale txtScale txtScale $ pictures
    [ -- 1. Lớp bóng đổ đen mờ tạo chiều sâu chữ
      translate 2.0 (-2.0) $ color (makeColorI 0 0 0 220) (renderGlyphs str)
      -- 2. Lớp chữ phát quang chính
    , color txtColor (renderGlyphs str)
    ]
  where
    renderGlyphs xs = fst (foldl' renderNext (blank, 0.0) xs)
    renderNext (!accPic, !curX) ch =
      let !vc = parseVnChar ch
          !cw = charWidth (vcBase vc)
          !gPic = renderSingleVnChar vc cw
      in (pictures [accPic, translate curX 0.0 gPic], curX + cw)

-- | Vẽ một ký tự đơn kèm dấu thanh tiếng Việt
renderSingleVnChar :: VnChar -> Float -> Picture
renderSingleVnChar !vc !cw =
  let !bChar = vcBase vc
      !isUpper = bChar >= 'A' && bChar <= 'Z'
      !midX    = cw / 2.0
      !topY    = if isUpper then 119.0 else 76.0
      !hasMu   = vcMod vc == VMu
      !markY   = if hasMu then topY + 28.0 else topY + 10.0
  in pictures
       [ -- Ký tự cơ sở ASCII
         text [bChar]

         -- Dấu mũ (Â, Ê, Ô)
       , case vcMod vc of
           VMu -> pictures
             [ line [ (midX - 14.0, topY + 6.0), (midX, topY + 24.0) ]
             , line [ (midX, topY + 24.0), (midX + 14.0, topY + 6.0) ]
             ]
           VTrang -> line [ (midX - 12.0, topY + 18.0), (midX, topY + 6.0), (midX + 12.0, topY + 18.0) ]
           VMoc   -> line [ (cw - 6.0, topY - 2.0), (cw + 6.0, topY + 16.0), (cw - 2.0, topY + 18.0) ]
           VBarD  -> if isUpper
                       then line [ (-10.0, 60.0), (32.0, 60.0) ]
                       else line [ (cw - 28.0, 78.0), (cw + 10.0, 78.0) ]
           VNothing -> blank

         -- Dấu thanh (Sắc, Huyền, Hỏi, Ngã, Nặng)
       , case vcTone vc of
           TSac   -> line [ (midX - 6.0, markY), (midX + 12.0, markY + 20.0) ]
           THuyen -> line [ (midX - 12.0, markY + 20.0), (midX + 6.0, markY) ]
           THoi   -> line [ (midX - 4.0, markY), (midX + 6.0, markY + 16.0), (midX + 2.0, markY + 20.0) ]
           TNga   -> line [ (midX - 12.0, markY + 6.0), (midX - 2.0, markY + 16.0), (midX + 10.0, markY + 10.0) ]
           TNang  -> translate midX (-18.0) (circleSolid 4.0)
           TNothing -> blank
       ]

-- ============================================================================
-- DỮ LIỆU MẪU MÔ PHỎNG (MOCK GAMESTATE) CHO DIỄN TẬP OFFLINE
-- ============================================================================

mockGameState :: GameState
mockGameState =
  let t1 = Tank 1 (Pos 4 4) North maxHealth 100 0 0 0 0 VanguardCyan
      t2 = Tank 999 (Pos 26 17) South maxHealth 50 0 0 0 0 TitanCrimson
      t3 = Tank 2 (Pos 26 4) West maxHealth 30 0 0 0 0 SolarisGold
      t4 = Tank 3 (Pos 4 17) East maxHealth 80 0 0 0 0 ViperEmerald
      b1 = Bullet (Pos 10 11) East 1 35
      b2 = Bullet (Pos 20 11) West 999 70
      o1 = PowerUp 1 (Pos 16 11) OverchargeOrb
      o2 = PowerUp 2 (Pos 8 8) HealOrb
      o3 = PowerUp 3 (Pos 23 13) EnergyOrb
      initScores = Map.fromList [(1, 140), (999, 90), (2, 40), (3, 60)]
      initialChat =
        [ ("Server", "Chao mung cac chien binh den voi Dau Truong 3D Hero Tank!")
        , ("Bot-Titan", "Phao kich da san sang, hay thu tai nao!")
        ]
  in GameState
       { tanks         = Map.fromList [(1, t1), (999, t2), (2, t3), (3, t4)]
       , bullets       = [b1, b2]
       , score         = initScores
       , powerUps      = [o1, o2, o3]
       , chatLog       = initialChat
       , gameTickCount = 0
       }

-- ============================================================================
-- GÓC XOAY CHUẨN XÁC THEO HƯỚNG BÀN CỜ GLOSS (ĐÃ SỬA LỖI NÒNG SÚNG NGƯỢC HƯỚNG)
-- ============================================================================

-- | Trong Gloss: rotate góc d quay theo CHIỀU KIM ĐỒNG HỒ
-- North: 0 độ (hướng lên +Y)
-- East:  90 độ (quay sang phải +X)
-- South: 180 độ (quay xuống dưới -Y)
-- West:  270 độ / -90 độ (quay sang trái -X)
targetAngle :: Direction -> Float
targetAngle dir = case dir of
  North -> 0.0
  East  -> 90.0
  South -> 180.0
  West  -> 270.0

-- ============================================================================
-- 1. BỘ MÔ HÌNH HÓA XE TĂNG 3D HỢP NHẤT (VẬN HÀNH ĐỒNG BỘ 1:1)
-- ============================================================================

-- | Khiên độc sinh học đặc thù của Viper Emerald khi kích hoạt Tuyệt Kỹ
drawViperNanoShield :: Float -> Picture
drawViperNanoShield !animTime =
  pictures
    [ rotate (animTime * 75.0) $ pictures
        [ color (makeColorI 40 255 100 180) (thickCircle 19.0 2.8)
        , color (makeColorI 180 255 200 70) (circleSolid 19.0)
        , color (makeColorI 40 255 100 220) $ pictures
            [ rotate (fromIntegral i * 60.0) $ translate 0.0 19.0 (rectangleSolid 4.0 4.0)
            | i <- [0..5 :: Int]
            ]
        ]
    , rotate (-animTime * 45.0) $
        color (makeColorI 20 200 80 120) (thickCircle 23.0 1.5)
    ]

drawHeroTank3D :: Float -> Float -> Tank -> Picture
drawHeroTank3D !animTime !currentAngle !tank =
  let !hInfo       = heroClassInfo (tankClass tank)
      !primaryC    = heroPrimaryCol hInfo
      !accentC     = heroAccentCol hInfo
      !glowC       = heroGlowCol hInfo
      !isDead      = tankRespawnTimer tank > 0
      !isInvinc    = tankInvincibleTimer tank > 0
      !blinkOn     = floor (animTime * 12.0) `mod` 2 == (0 :: Int)

      -- Hiệu ứng xích lăn sinh động khi di chuyển
      !treadOffset = sin (animTime * 18.0) * 2.5

      !bodyColor = if isDead then makeColorI 45 45 50 255 else primaryC
      !turretColor = if isDead then makeColorI 30 30 35 255 else accentC
  in pictures
       [ -- A. Bóng đổ 3D dưới gầm xe
         translate 2.5 (-3.5) $ color (makeColorI 0 0 0 140) (rectangleSolid 26.0 28.0)

         -- B. Toàn bộ thân xe xoay theo góc quay mượt mà (Heading Lerp)
       , rotate currentAngle $ pictures
           [ -- 1. Hai dải bánh xích kim loại có rãnh lăn
             translate (-11.5) 0.0 $ pictures
               [ color (makeColorI 20 24 30 255) (rectangleSolid 5.5 28.0)
               , color (makeColorI 55 65 78 255) (rectangleWire 5.5 28.0)
               , translate 0.0 treadOffset $ color (makeColorI 75 88 105 180) (rectangleSolid 4.0 2.0)
               ]
           , translate 11.5 0.0 $ pictures
               [ color (makeColorI 20 24 30 255) (rectangleSolid 5.5 28.0)
               , color (makeColorI 55 65 78 255) (rectangleWire 5.5 28.0)
               , translate 0.0 (-treadOffset) $ color (makeColorI 75 88 105 180) (rectangleSolid 4.0 2.0)
               ]

             -- 2. Thân xe đùn khối 3D (Chassis & Armor Bevel)
           , color (makeColorI 10 12 16 255) (rectangleSolid 19.0 24.0)
           , color bodyColor (rectangleSolid 17.5 22.0)
           , translate 0.0 2.0 $ color (makeColorI 255 255 255 70) (rectangleSolid 13.0 12.0)

             -- 3. Ống xả phản lực năng lượng đuôi
           , translate 0.0 (-10.0) $ pictures
               [ color glowC (rectangleSolid 9.0 2.5)
               , color white (rectangleSolid 5.0 1.2)
               ]

             -- 4. Nòng đại bác plasma vươn dài về phía trước (Hướng lên +Y)
           , translate 0.0 11.0 $ pictures
               [ color (makeColorI 15 18 24 255) (rectangleSolid 4.0 14.0)
               , color turretColor (rectangleSolid 2.8 13.0)
               , translate 0.0 6.5 $ color glowC (rectangleSolid 4.5 2.5)
               ]

             -- 5. Tháp pháo vòm trung tâm và lõi năng lượng
           , color (makeColorI 12 16 22 255) (circleSolid 7.5)
           , color turretColor (circleSolid 6.0)
           , color glowC (circleSolid 3.5)
           , color white (circleSolid 1.5)
           ]

         -- C. Khiên bảo vệ bất tử 2 giây chớp nháy (hoặc Khiên Độc Phục Hồi của Viper)
       , if isInvinc && blinkOn
           then if tankClass tank == ViperEmerald
                  then drawViperNanoShield animTime
                  else pictures
                         [ color (makeColorI 0 240 255 140) (thickCircle 17.0 2.5)
                         , color (makeColorI 180 255 255 70) (circleSolid 17.0)
                         ]
           else blank

         -- D. Xác xe cháy và đếm ngược hồi sinh
       , if isDead
           then pictures
                  [ translate 0.0 12.0 $ color (makeColorI 25 25 25 200) (circleSolid 9.0)
                  , translate (-3.0) 18.0 $ color (makeColorI 55 55 55 160) (circleSolid 7.0)
                  , translate 3.0 23.0 $ color (makeColorI 85 85 85 120) (circleSolid 5.0)
                  , translate (-10.0) (-5.0) $ scale 0.11 0.11 $
                      color (makeColorI 255 60 60 255) (text (show (tankRespawnTimer tank `div` 30 + 1) ++ "s"))
                  ]
           else blank

         -- E. Thanh Máu (HP Bar) và Năng lượng lơ lửng trên đầu
       , if not isDead
           then translate 0.0 18.0 $ pictures
                  [ color (makeColorI 10 12 16 220) (rectangleSolid 28.0 4.0)
                  , color (makeColorI 60 70 85 255) (rectangleWire 28.0 4.0)
                  , let hpRatio = fromIntegral (tankHealth tank) / fromIntegral maxHealth
                        hpW     = max 1.0 (26.0 * hpRatio)
                        hpCol   = if hpRatio > 0.5 then makeColorI 40 220 80 255
                                  else if hpRatio > 0.25 then makeColorI 240 200 30 255
                                  else makeColorI 255 40 40 255
                    in translate ((hpW - 26.0) / 2.0) 0.0 $ color hpCol (rectangleSolid hpW 2.6)
                  , translate 0.0 (-3.8) $ pictures
                      [ color (makeColorI 5 8 12 200) (rectangleSolid 28.0 2.2)
                      , let enRatio = fromIntegral (tankEnergy tank) / fromIntegral maxEnergy
                            enW     = max 0.5 (26.0 * enRatio)
                        in translate ((enW - 26.0) / 2.0) 0.0 $
                             color (makeColorI 0 220 255 255) (rectangleSolid enW 1.8)
                      ]
                  ]
           else blank
       ]

-- ============================================================================
-- 2. KẾT XUẤT MÀN HÌNH ĐĂNG NHẬP / CHỌN TƯỚNG (GỌN GÀNG, KHÔNG BỊ TRÀN VIỀN)
-- ============================================================================

drawLoginScreen
  :: Float
  -> String
  -> String
  -> String
  -> InputField
  -> TankClass
  -> Maybe String
  -> Picture
drawLoginScreen !animTime !callsign !host !port !activeField !currentCls !mErr =
  let hInfo = heroClassInfo currentCls
      dummyTank = Tank 0 (Pos 0 0) North maxHealth 100 0 0 0 0 currentCls
      shortName = case currentCls of
        VanguardCyan  -> "BĂNG GIÁ"
        TitanCrimson  -> "BẠO CHÚA"
        ViperEmerald  -> "ĐỘC XÀ"
        SolarisGold   -> "QUANG NĂNG"
        PhantomViolet -> "HƯ KHÔNG"
      offsetNameX = -fromIntegral (length shortName) * 4.2
  in pictures
       [ -- 1. Nền không gian số Cyber Grid
         drawCyberBackground animTime

         -- 2. Tiêu đề lớn căn giữa màn hình (Nằm gọn ở Y = 270)
       , translate 0.0 270.0 $ pictures
           [ translate (-260.0) 0.0 $
               drawVietnameseText 0.20 (makeColorI 0 240 255 255) "ĐẤU TRƯỜNG XE TĂNG 3D CYBER HERO"
           , translate (-180.0) (-24.0) $
               drawVietnameseText 0.10 (makeColorI 170 200 230 255) "ĐỒ ÁN CUỐI KỲ LẬP TRÌNH HÀM - NHÓM 5 THÀNH VIÊN"
           ]

         -- 3. THẺ TRÁI: ĐIỀU KHIỂN & KẾT NỐI (Rộng 330, Cao 430, căn tại X = -265, Y = -20)
       , translate (-265.0) (-20.0) $ pictures
           [ color (makeColorI 14 18 28 235) (rectangleSolid 330.0 430.0)
           , color (makeColorI 0 220 255 180) (rectangleWire 330.0 430.0)

             -- Tiêu đề bảng
           , translate (-135.0) 180.0 $
               drawVietnameseText 0.125 (makeColorI 255 215 0 255) "KHOANG ĐIỀU KHIỂN PHI CÔNG"

             -- Ô nhập Callsign
           , translate (-135.0) 135.0 $
               drawVietnameseText 0.095 (makeColorI 200 220 240 255) "HÔ HIỆU PHI CÔNG (CALLSIGN):"
           , translate 0.0 105.0 $
               drawInputField (activeField == FieldCallsign) callsign "COMMANDER" animTime 280.0

             -- Ô nhập Server Host
           , translate (-135.0) 60.0 $
               drawVietnameseText 0.095 (makeColorI 200 220 240 255) "MÁY CHỦ (HOST IP):"
           , translate 0.0 30.0 $
               drawInputField (activeField == FieldServerHost) host "127.0.0.1" animTime 280.0

             -- Ô nhập Server Port
           , translate (-135.0) (-15.0) $
               drawVietnameseText 0.095 (makeColorI 200 220 240 255) "CỔNG KẾT NỐI (PORT):"
           , translate 0.0 (-45.0) $
               drawInputField (activeField == FieldServerPort) port "9123" animTime 280.0

             -- Báo lỗi
           , case mErr of
               Just err -> translate (-135.0) (-78.0) $
                 drawVietnameseText 0.085 (makeColorI 255 70 70 255) ("Lỗi: " ++ err)
               Nothing  -> blank

             -- Nút [ THAM CHIẾN ONLINE ]
           , translate 0.0 (-115.0) $ pictures
               [ color (makeColorI 0 160 220 255) (rectangleSolid 280.0 36.0)
               , color (makeColorI 180 245 255 255) (rectangleWire 280.0 36.0)
               , translate (-110.0) (-6.0) $
                   drawVietnameseText 0.11 white "[ THAM CHIẾN - TRỰC TUYẾN ]"
               ]

             -- Nút [ DIỄN TẬP OFFLINE ]
           , translate 0.0 (-165.0) $ pictures
               [ color (makeColorI 30 45 65 240) (rectangleSolid 280.0 34.0)
               , color (makeColorI 100 150 200 200) (rectangleWire 280.0 34.0)
               , translate (-105.0) (-5.0) $
                   drawVietnameseText 0.105 (makeColorI 180 210 240 255) "[ DIỄN TẬP NGOẠI TUYẾN - F1 ]"
               ]
           ]

         -- 4. THẺ PHẢI: PHÒNG TRƯNG BÀY XE TĂNG & KỸ NĂNG TIẾNG VIỆT (Rộng 570, Cao 430, X = 200, Y = -20)
       , translate 200.0 (-20.0) $ pictures
           [ color (makeColorI 14 18 28 235) (rectangleSolid 570.0 430.0)
           , color (heroPrimaryCol hInfo) (rectangleWire 570.0 430.0)

             -- Tên danh xưng Chiến binh tiếng Việt có dấu
           , translate (-255.0) 180.0 $
               drawVietnameseText 0.155 (heroPrimaryCol hInfo) (heroTitle hInfo)

             -- Vai trò chiến thuật
           , translate (-255.0) 156.0 $
               drawVietnameseText 0.095 (makeColorI 180 200 220 255) (heroRole hInfo)

             -- BỆ PHÓNG 3D XOAY TRÒN BÊN TRÁI
           , translate (-150.0) 48.0 $ pictures
               [ color (makeColorI 10 25 40 255) (circleSolid 65.0)
               , color (heroGlowCol hInfo) (thickCircle 65.0 2.5)
               , scale 2.0 2.0 (drawHeroTank3D animTime (animTime * 45.0) dummyTank)
               ]

             -- Nút chuyển đổi Hero (< [A] ... [D] >) đặt ngay dưới bệ 3D, căn giữa rộng rãi không đè chữ
           , translate (-150.0) (-45.0) $ pictures
               [ translate (-95.0) (-6.0) $ drawVietnameseText 0.115 (makeColorI 0 240 255 255) "< [A]"
               , translate offsetNameX (-6.0) $ drawVietnameseText 0.11 white shortName
               , translate 60.0 (-6.0) $ drawVietnameseText 0.115 (makeColorI 0 240 255 255) "[D] >"
               ]

             -- KHỐI CHỈ SỐ CƠ BẢN (BÊN PHẢI BỆ XOAY)
           , translate 75.0 115.0 $ pictures
               [ drawStatBar "Máu (HP)" (heroStatHp hInfo) (makeColorI 40 220 80 255)
               , translate 0.0 (-26.0) $ drawStatBar "Tốc độ" (heroStatSpd hInfo) (makeColorI 0 220 255 255)
               , translate 0.0 (-52.0) $ drawStatBar "Hỏa lực" (heroStatAtk hInfo) (makeColorI 255 50 50 255)
               , translate 0.0 (-78.0) $ drawStatBar "Phòng thủ" (heroStatDef hInfo) (makeColorI 255 200 20 255)
               ]

             -- THẺ MÔ TẢ TUYỆT KỸ [E] TIẾNG VIỆT (NẰM DƯỚI CÙNG, RỘNG 520, KHÔNG BỊ ĐÈ CHỮ)
           , translate 0.0 (-125.0) $ pictures
               [ color (makeColorI 18 24 38 255) (rectangleSolid 520.0 80.0)
               , color (heroGlowCol hInfo) (rectangleWire 520.0 80.0)
               , translate (-245.0) 22.0 $
                   drawVietnameseText 0.105 (makeColorI 255 215 0 255) ("TUYỆT KỸ [E]: " ++ heroUltName hInfo)
               , translate (-245.0) 2.0 $
                   drawVietnameseText 0.086 white (heroUltDesc1 hInfo)
               , translate (-245.0) (-18.0) $
                   drawVietnameseText 0.086 white (heroUltDesc2 hInfo)
               ]

             -- Hướng dẫn phím chia làm 2 dòng cân đối tuyệt đối, không tràn biên
           , translate (-255.0) (-188.0) $
               drawVietnameseText 0.082 (makeColorI 160 190 220 255)
                 "[A] / [D] Đổi Hero  •  [1..5] Chọn nhanh lớp tướng"
           , translate (-255.0) (-206.0) $
               drawVietnameseText 0.082 (makeColorI 160 190 220 255)
                 "[Enter] Vào trận đấu  •  [F1] Diễn tập offline  •  [Tab] Đổi ô"
           ]
       ]

drawInputField :: Bool -> String -> String -> Float -> Float -> Picture
drawInputField !isActive !val !placeholder !animTime !w =
  let blinkCursor = isActive && (floor (animTime * 3.0) `mod` 2 == (0 :: Int))
      displayText = if null val && not isActive then placeholder else val
      textColor   = if null val && not isActive then makeColorI 110 130 160 255 else white
      borderColor = if isActive then makeColorI 0 240 255 255 else makeColorI 60 80 110 200
  in pictures
       [ color (makeColorI 10 14 22 255) (rectangleSolid w 32.0)
       , color borderColor (rectangleWire w 32.0)
       , translate (-w / 2.0 + 12.0) (-5.0) $ scale 0.095 0.095 $ color textColor (text displayText)
       , if blinkCursor
           then let cursorX = -w / 2.0 + 12.0 + fromIntegral (length val) * 10.0
                in translate cursorX (-5.0) $ color (makeColorI 0 240 255 255) (rectangleSolid 2.0 14.0)
           else blank
       ]

drawStatBar :: String -> Float -> Color -> Picture
drawStatBar !label !ratio !barCol =
  pictures
    [ translate (-80.0) (-4.0) $
        drawVietnameseText 0.08 (makeColorI 180 200 220 255) label
    , translate 45.0 0.0 $ pictures
        [ color (makeColorI 10 15 25 255) (rectangleSolid 110.0 10.0)
        , color (makeColorI 50 65 90 255) (rectangleWire 110.0 10.0)
        , let w = 106.0 * ratio
          in translate ((w - 106.0) / 2.0) 0.0 $ color barCol (rectangleSolid w 7.0)
        ]
    ]

drawCyberBackground :: Float -> Picture
drawCyberBackground !animTime =
  pictures
    [ color (makeColorI 7 10 16 255) (rectangleSolid 1400.0 800.0)
    , pictures
        [ translate x 0.0 $ color (makeColorI 0 180 255 18) (rectangleSolid 1.0 800.0)
        | i <- [-20 .. 20 :: Int]
        , let x = fromIntegral i * 35.0
        ]
    , pictures
        [ translate 0.0 y $ color (makeColorI 0 180 255 18) (rectangleSolid 1400.0 1.0)
        | j <- [-12 .. 12 :: Int]
        , let y = fromIntegral j * 35.0 + fromIntegral (floor (animTime * 15.0) `mod` (35 :: Int) :: Int)
        ]
    ]

-- ============================================================================
-- 3. KẾT XUẤT CHIẾN TRƯỜNG IN-GAME 3D (32x22 BẢO ĐẢM KHÔNG BỊ CẮT XÉN)
-- ============================================================================

draw3DPlayingScene
  :: Float
  -> Int
  -> String
  -> GameState
  -> Map.Map Int (Float, Float)
  -> Map.Map Int Float           -- ^ Góc xoay nòng mượt mà của từng xe
  -> [Particle]
  -> [Shockwave]
  -> Bool
  -> Bool
  -> Picture
draw3DPlayingScene !animTime !myId !callsign !st !visualTanks !visualAngles !particles !shockwaves !isOnline !isMuted =
  pictures
    [ -- 1. Nền bản đồ chiến trường 32x22
      drawArenaFloor

      -- 2. Vẽ ngọc sức mạnh 3D
    , pictures (map (drawPowerUp3D animTime) (powerUps st))

      -- 3. Vẽ khối tường đá 3D
    , pictures (map drawWall3D walls)

      -- 4. Vẽ các viên đạn / tên lửa / chùm tia năng lượng với hiệu ứng riêng biệt
    , pictures (map (drawBullet3D animTime) (bullets st))

      -- 5. Vẽ xe tăng với tọa độ Lerp và góc xoay nòng chính xác
    , pictures
        [ let (!sx, !sy) = Map.findWithDefault (toScreenPos (tankPos t)) (tankId t) visualTanks
              !curAngle  = Map.findWithDefault (targetAngle (tankDir t)) (tankId t) visualAngles
          in translate sx sy (drawHeroTank3D animTime curAngle t)
        | t <- Map.elems (tanks st)
        ]

      -- 6. Hiệu ứng hạt và sóng nổ
    , pictures (map drawParticle particles)
    , pictures (map (drawShockwave animTime) shockwaves)

      -- 7. Giao diện HUD, Minimap, Skill Dock (Đã loại bỏ khung chat hoàn toàn để màn hình thông thoáng)
    , drawInGameHUD animTime myId callsign st isOnline isMuted
    ]

drawArenaFloor :: Picture
drawArenaFloor =
  pictures
    [ color (makeColorI 5 7 11 255) (rectangleSolid 1400.0 800.0)
    , color (makeColorI 14 18 26 255) (rectangleSolid (arenaHalfW * 2.0) (arenaHalfH * 2.0))
    , pictures
        [ translate x y $ color (makeColorI 25 32 45 120) (rectangleWire cellSize cellSize)
        | ix <- [0 .. gridWidth - 1]
        , iy <- [0 .. gridHeight - 1]
        , let (x, y) = toScreenPos (Pos ix iy)
        ]
    , color (makeColorI 0 200 255 180) (rectangleWire (arenaHalfW * 2.0 + 2.0) (arenaHalfH * 2.0 + 2.0))
    ]

drawWall3D :: Position -> Picture
drawWall3D !pos =
  let (!x, !y) = toScreenPos pos
      w = cellSize - 2.0
  in translate x y $ pictures
       [ translate 2.0 (-2.5) $ color (makeColorI 0 0 0 140) (rectangleSolid w w)
       , color (makeColorI 30 38 52 255) (rectangleSolid w w)
       , translate (-1.0) 1.0 $ pictures
           [ color (makeColorI 55 68 88 255) (rectangleSolid (w - 2.5) (w - 2.5))
           , color (makeColorI 120 150 190 200) (rectangleWire (w - 2.5) (w - 2.5))
           , color (makeColorI 20 28 40 255) (circleSolid 3.0)
           ]
       ]

-- | Vẽ đạn pháo với hiệu ứng đồ họa riêng biệt cho từng kỹ năng và súng thường:
-- 1. Súng thường (dmg <= 45): Đạn plasma tinh gọn, sắc sảo, không gây rối mắt
-- 2. Pháo kích Titan (dmg == 75): Hỏa tiễn hạng nặng bọc thép, cánh đuôi và luồng phụt lửa
-- 3. Chùm tia Solaris (dmg >= 100): Phân đoạn laze quang năng rực rỡ, nối liền thành chùm tia xuyên bản đồ
drawBullet3D :: Float -> Bullet -> Picture
drawBullet3D !animTime !b =
  let (!x, !y) = toScreenPos (bulletPos b)
      !dmg = bulletDamage b
      !dirAngle = targetAngle (bulletDir b)
  in translate x y $ rotate dirAngle $
       if dmg >= 100
         then -- SOLARIS HYPERION BEAM: Chùm tia laze quang năng hủy diệt
           pictures
             [ color (makeColorI 255 210 20 180) (rectangleSolid 14.0 cellSize)
             , color (makeColorI 255 240 100 240) (rectangleSolid 8.0 cellSize)
             , color white (rectangleSolid 3.5 cellSize)
             , color (makeColorI 255 255 200 200) (circleSolid 5.0)
             ]
         else if dmg >= 65
           then -- TITAN CRIMSON BARRAGE: Hỏa tiễn hạng nặng với luồng phụt lửa
             let flamePulse = 6.0 + sin (animTime * 35.0) * 2.0
             in pictures
                  [ -- Luồng lửa phản lực đuôi
                    translate 0.0 (-11.0) $ pictures
                      [ color (makeColorI 255 120 20 220) (rectangleSolid 4.5 flamePulse)
                      , color (makeColorI 255 230 40 255) (rectangleSolid 2.5 (flamePulse * 0.7))
                      ]
                    -- Cánh lái đuôi (Delta Fins)
                  , color (makeColorI 80 15 20 255) $ pictures
                      [ translate (-4.5) (-5.0) (rectangleSolid 3.0 6.0)
                      , translate 4.5 (-5.0) (rectangleSolid 3.0 6.0)
                      ]
                    -- Thân tên lửa bọc thép
                  , color (makeColorI 45 48 55 255) (rectangleSolid 6.0 14.0)
                  , color (makeColorI 220 35 45 255) (rectangleSolid 4.5 12.0)
                    -- Đầu đạn xuyên phá
                  , translate 0.0 7.5 $ color (makeColorI 255 220 30 255) (circleSolid 3.0)
                  ]
           else -- STANDARD PLASMA BOLT: Viên đạn plasma tinh gọn, sắc sảo
             pictures
               [ color (makeColorI 0 240 255 180) (rectangleSolid 5.0 12.0)
               , color (makeColorI 170 245 255 240) (rectangleSolid 3.0 9.0)
               , color white (rectangleSolid 1.5 5.0)
               , translate 0.0 4.0 $ color (makeColorI 200 255 255 255) (circleSolid 2.0)
               ]

drawPowerUp3D :: Float -> PowerUp -> Picture
drawPowerUp3D !animTime !orb =
  let (!x, !y) = toScreenPos (orbPos orb)
      !bounceY = y + sin (animTime * 4.0 + fromIntegral (orbId orb)) * 3.0
      !orbCol = case orbType orb of
        HealOrb       -> makeColorI 40 240 90 255
        EnergyOrb     -> makeColorI 0 220 255 255
        OverchargeOrb -> makeColorI 255 210 20 255
  in translate x bounceY $ pictures
       [ rotate (animTime * 60.0) $ pictures
           [ color (makeColorI 255 255 255 60) (rectangleWire 16.0 16.0)
           , color orbCol (thickCircle 9.0 1.8)
           ]
       , color orbCol (circleSolid 7.0)
       , color white (circleSolid 3.0)
       ]

drawParticle :: Particle -> Picture
drawParticle !p =
  let a = max 0.0 (min 1.0 (partLife p))
  in translate (partX p) (partY p) $
       color (partColor p) (circleSolid (partSize p * a))

drawShockwave :: Float -> Shockwave -> Picture
drawShockwave !animTime !sw =
  let a = max 0.0 (min 1.0 (shockLife sw))
      r = shockRadius sw
      col = shockColor sw
  in translate (shockX sw) (shockY sw) $ pictures
       [ -- Vòng sóng xung kích chính
         color col (thickCircle r (max 1.2 (3.2 * a)))
       , -- Hiệu ứng gai tinh thể băng nếu là Cryo Nova (sóng bán kính lớn)
         if r > 18.0 && a > 0.15
           then rotate (animTime * 45.0) $ pictures
                  [ rotate (fromIntegral i * 60.0) $
                      translate 0.0 r $ color col (rectangleSolid 3.0 7.0)
                  | i <- [0..5 :: Int]
                  ]
           else blank
       ]

-- ============================================================================
-- 4. KẾT XUẤT HUD IN-GAME (KHÔNG BỊ TRÀN VIỀN MÀN HÌNH, BỐ CỤC CÂN ĐỐI)
-- ============================================================================

drawInGameHUD
  :: Float
  -> Int
  -> String
  -> GameState
  -> Bool
  -> Bool
  -> Picture
drawInGameHUD !animTime !myId !callsign !st !isOnline !isMuted =
  let myTankOpt = Map.lookup myId (tanks st)
      myScore   = Map.findWithDefault 0 myId (score st)
  in pictures
       [ -- A. THANH TRẠNG THÁI TRÊN CÙNG (Y = 325)
         translate 0.0 325.0 $ pictures
           [ color (makeColorI 12 16 24 235) (rectangleSolid 1240.0 34.0)
           , color (makeColorI 0 200 255 180) (rectangleWire 1240.0 34.0)
           , translate (-590.0) (-6.0) $
               drawVietnameseText 0.105 (makeColorI 0 240 255 255) ("PHI CÔNG: " ++ callsign ++ " [#" ++ show myId ++ "]")
           , translate (-330.0) (-6.0) $
               drawVietnameseText 0.105 (makeColorI 255 215 0 255) ("ĐIỂM: " ++ show myScore)
           , translate (-170.0) (-6.0) $
               drawVietnameseText 0.10 (if isOnline then makeColorI 50 255 100 255 else makeColorI 255 180 30 255)
                 (if isOnline then "MÁY CHỦ: ĐÃ KẾT NỐI (ONLINE)" else "CHẾ ĐỘ: DIỄN TẬP OFFLINE")
           , translate 160.0 (-6.0) $
               drawVietnameseText 0.10 (if isMuted then makeColorI 255 70 70 255 else makeColorI 100 255 150 255)
                 (if isMuted then "[M] ÂM THANH: TẮT" else "[M] ÂM THANH: BẬT")
           , translate 350.0 (-6.0) $
               drawVietnameseText 0.095 (makeColorI 180 200 230 255) "[WASD] Di chuyển | [Space] Bắn | [E] Tuyệt kỹ | [1..5] Đổi Hero"
           ]

         -- B. RADAR MINIMAP GÓC TRÊN BÊN PHẢI (NẰM GỌN Ở X = 510, Y = 205)
       , translate 510.0 205.0 (drawMinimap st myId)

         -- C. KHỐI SKILL DOCK Ở ĐÁY MÀN HÌNH (RỘNG RÃI 680px, Y = -320)
       , case myTankOpt of
           Just myTank -> translate 0.0 (-320.0) (drawSkillDock animTime myTank)
           Nothing     -> blank
       ]

drawMinimap :: GameState -> Int -> Picture
drawMinimap !st !myId =
  let mw = 140.0
      mh = 96.0
      toMiniX x = fromIntegral x * (mw / fromIntegral gridWidth) - mw / 2.0
      toMiniY y = fromIntegral y * (mh / fromIntegral gridHeight) - mh / 2.0
  in pictures
       [ color (makeColorI 10 15 22 230) (rectangleSolid mw mh)
       , color (makeColorI 0 220 255 180) (rectangleWire mw mh)
       , translate (-60.0) 36.0 $ scale 0.07 0.07 $ color (makeColorI 0 240 255 255) (text "RADAR 32x22")
       , pictures
           [ translate (toMiniX (posX w)) (toMiniY (posY w)) $
               color (makeColorI 60 75 95 255) (rectangleSolid 3.5 3.5)
           | w <- walls
           ]
       , pictures
           [ translate (toMiniX (posX (orbPos o))) (toMiniY (posY (orbPos o))) $
               color (makeColorI 255 220 30 255) (circleSolid 2.0)
           | o <- powerUps st
           ]
       , pictures
           [ translate (toMiniX (posX (tankPos t))) (toMiniY (posY (tankPos t))) $
               if tankId t == myId
                 then color (makeColorI 0 255 100 255) (circleSolid 3.0)
                 else color (makeColorI 255 50 50 255) (circleSolid 2.5)
           | t <- Map.elems (tanks st)
           ]
       ]

-- | Bảng điều khiển Kỹ năng & Chỉ số Máu / Năng lượng thiết kế rộng rãi 680px,
-- chữ hiển thị cân đối hoàn toàn bên trong thanh, tuyệt đối không tràn viền
drawSkillDock :: Float -> Tank -> Picture
drawSkillDock !animTime !tank =
  let hInfo     = heroClassInfo (tankClass tank)
      isReady   = tankEnergy tank >= maxEnergy
      blinkGlow = isReady && (floor (animTime * 8.0) `mod` 2 == (0 :: Int))
      barW      = 340.0
  in pictures
       [ color (makeColorI 12 16 25 240) (rectangleSolid 680.0 60.0)
       , color (if isReady then makeColorI 255 215 0 255 else makeColorI 0 200 255 180)
               (rectangleWire 680.0 60.0)

         -- 1. Biểu tượng [E] Tuyệt Kỹ
       , translate (-290.0) 0.0 $ pictures
           [ color (makeColorI 20 28 42 255) (circleSolid 22.0)
           , color (if isReady then makeColorI 255 215 0 255 else makeColorI 70 90 120 255) (thickCircle 22.0 2.5)
           , if blinkGlow then color (makeColorI 255 230 40 85) (circleSolid 22.0) else blank
           , translate (-8.0) (-8.0) $
               drawVietnameseText 0.15 (if isReady then makeColorI 255 220 0 255 else white) "[E]"
           ]

         -- 2. Tên Tuyệt Kỹ & Trạng thái tích nạp
       , translate (-250.0) 11.0 $
           drawVietnameseText 0.105 (heroPrimaryCol hInfo) (heroUltName hInfo)
       , translate (-250.0) (-11.0) $
           drawVietnameseText 0.085 (if isReady then makeColorI 255 220 30 255 else makeColorI 180 200 220 255)
             (if isReady then ">>> SẴN SÀNG! NHẤN [E] <<<"
                         else "Tích nạp: " ++ show (tankEnergy tank) ++ " / 100%")

         -- 3. Thanh Máu (HP Bar 600 HP rộng 340px)
       , translate 115.0 12.0 $ pictures
           [ color (makeColorI 8 12 18 255) (rectangleSolid barW 20.0)
           , color (makeColorI 60 75 95 255) (rectangleWire barW 20.0)
           , let ratio  = fromIntegral (tankHealth tank) / fromIntegral maxHealth
                 fillW  = max 2.0 ((barW - 4.0) * ratio)
                 barCol = if ratio > 0.5 then makeColorI 40 220 80 255
                          else if ratio > 0.25 then makeColorI 240 200 30 255
                          else makeColorI 255 45 45 255
             in translate ((fillW - (barW - 4.0)) / 2.0) 0.0 $ color barCol (rectangleSolid fillW 16.0)
           , translate (-65.0) (-4.0) $
               drawVietnameseText 0.082 white ("MÁU (HP): " ++ show (tankHealth tank) ++ " / 600")
           ]

         -- 4. Thanh Năng Lượng / Cơn Thịnh Nộ (Rage Bar rộng 340px)
       , translate 115.0 (-12.0) $ pictures
           [ color (makeColorI 8 12 18 255) (rectangleSolid barW 18.0)
           , color (makeColorI 60 75 95 255) (rectangleWire barW 18.0)
           , let ratio  = fromIntegral (tankEnergy tank) / fromIntegral maxEnergy
                 fillW  = max 2.0 ((barW - 4.0) * ratio)
                 barCol = if isReady then makeColorI 255 215 0 255 else makeColorI 0 220 255 255
             in translate ((fillW - (barW - 4.0)) / 2.0) 0.0 $ color barCol (rectangleSolid fillW 14.0)
           , translate (-95.0) (-3.5) $
               drawVietnameseText 0.08 white ("NĂNG LƯỢNG CHIẾN ĐẤU: " ++ show (tankEnergy tank) ++ "%")
           ]
       ]
