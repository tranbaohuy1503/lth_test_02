# MULTIPLAYER TANK BATTLE ARENA (CLIENT-SERVER 2D REALTIME GAME)
> **Đồ án cuối kỳ môn Lập trình hàm (Functional Programming)**  
> **Kiến trúc phân rã 5 thành viên - Hoàn chỉnh, chuẩn hóa 100% giáo trình, biên dịch và chạy thực nghiệm tức thì.**

---

## 1. TỔNG QUAN HỆ THỐNG (SYSTEM OVERVIEW)
- **Tên trò chơi:** Multiplayer Tank Battle Arena
- **Thể loại:** Game hành động bắn xe tăng 2D thời gian thực (Realtime Client-Server Architecture).
- **Bản đồ chiến trường:** Lưới ô vuông $20 \times 20$ (Grid-based Cartesian 2D) có chướng ngại vật tường đá (Stone Walls) đối xứng.
- **Mục tiêu:** Điều khiển xe tăng di chuyển, quay nòng súng, né tránh đạn và tiêu diệt các xe tăng đối thủ (người chơi khác hoặc Bot AI thông minh) để tích lũy điểm số cao nhất trên Bảng xếp hạng.

---

## 2. BẢNG PHÂN CÔNG TRÁCH NHIỆM 5 THÀNH VIÊN (TEAM MATRIX)

| Thành viên | File Phụ trách | Khái niệm & Bài giảng áp dụng (Pedagogical Citations) | Vai trò chính |
| :--- | :--- | :--- | :--- |
| **THÀNH VIÊN 1** | `src/Types.hs`<br>`src/GameLogic.hs` | **[Buoi 01, 03 & Chap 6 ADT, Chap 7 Strictness]**<br>• Pure Functions (Referential Transparency)<br>• Higher-Order Functions (`map`, `filter`, `foldl'`)<br>• ADTs (Sum/Product Types, Record Syntax)<br>• Strictness Control (`{-# LANGUAGE BangPatterns #-}`, `!`) | Định nghĩa thực thể thế giới game; tính toán di chuyển, bắn đạn, kiểm tra va chạm đạn-tường, đạn-xe và hồi sinh an toàn bằng hàm thuần túy. |
| **THÀNH VIÊN 2** | `src/Protocol.hs`<br>`src/Server/Network.hs` | **[Chap 11 Networking & Chap 7 Either Monad]**<br>• Line-Based Message Framing (`\n`)<br>• Monadic Validation Pipeline (`Either String` Monad)<br>• TCP Socket Lifecycle (`getAddrInfo`, `bind`, `listen`, `accept`)<br>• Buffered Line Receiver & Safe Broadcast | Xây dựng đường truyền mạng; mã hóa/giải mã và kiểm thử tính hợp lệ ngữ nghĩa của gói tin; quản lý Socket và phát sóng trạng thái thế giới. |
| **THÀNH VIÊN 3** | `src/Server/State.hs`<br>`src/Server/Tick.hs` | **[Chap 5 Thread & Chap 6 Synchronization]**<br>• Software Transactional Memory (STM)<br>• `TVar GameState`, `atomically`, `modifyTVar'`<br>• STM Mailbox Pattern (`TChan`)<br>• Write Aggregation / Command Batching<br>• 30 FPS Heartbeat Loop (`threadDelay 33333`) | Quản lý bộ nhớ tập trung chống Race Condition/Deadlock; tạo nhịp tim 30 FPS điều phối gom cụm lệnh và phát sóng thế giới. |
| **THÀNH VIÊN 4** | `src/Server/Supervisor.hs`<br>`src/AI.hs` | **[Chap 7 Supervision & Haskell #2 Sedma Assignment]**<br>• Concurrency Supervision (`async`, `waitCatch`, `withAsync`)<br>• Resource Safety (`bracket`, `finally`, `catch`)<br>• Client Timeout Detection (`System.Timeout`)<br>• Supervisor Restart Loop (tự phục hồi Bot worker)<br>• Pure Decision State Machine (`BotMemory`, khoảng cách Manhattan) | Giám sát luồng mạng, tự dọn dẹp tài nguyên khi người chơi ngắt kết nối; lập trình Bot AI tự săn lùng và xả đạn đối thủ. |
| **THÀNH VIÊN 5** | `src/Client/UI.hs`<br>`src/Client/Main.hs` | **[Chương 6 ADT & Chương 7 Strictness & Gloss 2.5D]**<br>• Cỗ máy dựng hình 3D/2.5D Extruded Voxel đa tầng<br>• Màn hình Login/Lobby hiện đại với Showroom 3D xoay vòng<br>• Bộ chọn Skin vỏ xe tăng (CyberCyan, CrimsonMecha, EmeraldViper, SolarGold)<br>• Hệ thống hạt tàn lửa 3D (Particle System) & Sóng xung kích nổ<br>• Phép nội suy làm mịn tọa độ (Lerp) ở 60 FPS chống giật khựng<br>• Dashboard Esports kính mờ, Mini-Radar chiến thuật và Huy chương (#1 Gold, #2 Silver, #3 Bronze) | Lập trình toàn diện giao diện đồ họa 3D phía Client, phòng chờ đăng nhập và tương tác phím/chuột mượt mà không rò rỉ bộ nhớ. |

---

## 3. CẤU TRÚC THƯ MỤC DỰ ÁN (PROJECT DIRECTORY TREE)

```text
tank-battle/
├── tank-battle.cabal          # File cấu hình đóng gói Cabal (tank-server & tank-client)
├── cabal.project              # Cấu hình dự án Cabal chuẩn hóa
├── freeglut.dll               # Thư viện đồ họa Windows Native hỗ trợ Gloss 2D/3D
├── build.bat                  # Script biên dịch tự động nhanh (Command Prompt / PowerShell)
├── run-server.bat             # Script khởi động Máy chủ chiến trường
├── run-client.bat             # Script khởi động Client đồ họa AAA
├── README.md                  # Tài liệu kiến trúc và hướng dẫn sử dụng chi tiết
└── src/
    ├── Types.hs               # [DÙNG CHUNG] 6 ADT cốt lõi, tường đá & hằng số bản đồ 20x20
    ├── GameLogic.hs           # [THÀNH VIÊN 1] Logic thuần túy, va chạm & hồi sinh
    ├── Protocol.hs            # [THÀNH VIÊN 2] Mã hóa/giải mã, Monadic Validation Pipeline
    ├── AI.hs                  # [THÀNH VIÊN 4] Bot AI tự hành (Manhattan Distance & Patrol)
    ├── Server/
    │   ├── State.hs           # [THÀNH VIÊN 3] Quản lý bộ nhớ STM & TChan Mailbox
    │   ├── Network.hs         # [THÀNH VIÊN 2] TCP Server, Line Buffer & Broadcast
    │   ├── Tick.hs            # [THÀNH VIÊN 3] Heartbeat Loop 30 FPS gom cụm lệnh
    │   ├── Supervisor.hs      # [THÀNH VIÊN 4] Giám sát chịu lỗi, Timeout & Restart Loop
    │   └── Main.hs            # Điều phối tiến trình máy chủ
    └── Client/
        ├── UI.hs              # [THÀNH VIÊN 5] 3D Voxel Engine, Login/Lobby & Dashboard HUD
        └── Main.hs            # [THÀNH VIÊN 5] Vòng lặp đồ họa playIO, Lerp 60 FPS & Particle Simulation
```

---

## 4. HƯỚNG DẪN BIÊN DỊCH VÀ CHẠY THỰC NGHIỆM (QUICKSTART)

### Bước 1: Biên dịch toàn bộ dự án
Mở cửa sổ Terminal (PowerShell hoặc CMD) tại thư mục dự án và chạy:
```powershell
.\build.bat
```
*(Hoặc dùng lệnh Cabal trực tiếp: `cabal build --builddir=C:\cabal-build\tank-battle`)*

---

### Bước 2: Khởi chạy Máy chủ (Server)
Chạy server trên cổng mặc định (9123):
```powershell
.\run-server.bat
```

---

### Bước 3: Khởi chạy Người chơi (Clients)
Mở cửa sổ Terminal mới để khởi chạy Client:
```powershell
.\run-client.bat
```
Cửa sổ đồ họa độ phân giải $1080 \times 720$ sẽ xuất hiện với **Màn hình Đăng nhập (Login/Lobby Screen)**:
1. **Pilot Callsign:** Nhập tên phi công trực tiếp từ bàn phím.
2. **Server Host & Port:** Mặc định `127.0.0.1` và `9123` (có thể bấm chuột hoặc phím `TAB` / Mũi tên lên-xuống để sửa).
3. **Showroom 3D:** Bấm `<` `>` hoặc `TAB` để đổi màu Skin xe tăng (`CyberCyan`, `CrimsonMecha`, `EmeraldViper`, `SolarGold`) và quan sát mô hình xe 3D xoay vòng trực tiếp.
4. **Vào trận:**
   - Bấm nút **`[ ENTER ARENA - ONLINE ]`** (hoặc gõ phím `Enter`) để kết nối Server.
   - Bấm nút **`[ PRACTICE ARENA - OFFLINE ]`** (hoặc gõ phím `F1`) để diễn tập offline độc lập.

---

## 5. BÀN PHÍM VÀ CHUỘT ĐIỀU KHIỂN (CONTROLS)

### Tại Màn hình Đăng nhập (Lobby):
- **Gõ ký tự / Backspace:** Nhập tên phi công, IP, Port.
- **`TAB` / Mũi tên Lên-Xuống:** Chuyển đổi giữa các ô nhập liệu.
- **Mũi tên Trái-Phải / Phím `<` `>`:** Đổi giao diện Skin xe tăng 3D.
- **Chuột Trái:** Click trực tiếp vào các nút bấm hoặc ô nhập liệu.
- **`Enter`:** Kết nối Server Online.
- **`F1`:** Chơi Diễn tập Offline.

### Trong Trận Đấu (Battle Arena):
- **`W` hoặc Mũi tên Lên:** Di chuyển lên phía Bắc (North).
- **`S` hoặc Mũi tên Xuống:** Di chuyển xuống phía Nam (South).
- **`A` hoặc Mũi tên Trái:** Di chuyển sang phía Tây (West).
- **`D` hoặc Mũi tên Phải:** Di chuyển sang phía Đông (East).
- **`Spacebar` (Phím cách):** Bắn đạn đại bác Plasma.
- **`TAB`:** Đổi nhanh Skin màu xe tăng ngay trong trận.
- **`ESC`:** Thoát khỏi trận đấu, quay về phòng chờ Login/Lobby.
