------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Real_Time;      use Ada.Real_Time;
with Numworks;           use Numworks;
with Numworks.Backlight;
with Numworks.Display;
with Numworks.Keyboard;

with HAL.Bitmap;
with Bitmapped_Drawing;  use Bitmapped_Drawing;
with BMP_Fonts;

procedure Main is
   X : Integer := 50;
   Y : Integer := 50;
   Radius : Integer := 5;
   Color : HAL.Bitmap.Bitmap_Color := HAL.Bitmap.Red;
   FPS, FPS_Count : Natural := 0;
   Last_FPS_Count : Time := Time_First;
begin
   Display.Set_Drawing_Area (((0, 0), 320, 240));
   Backlight.Set_Level (16);
   Display.Buffer.Set_Source (HAL.Bitmap.White);
   Display.Buffer.Fill;
   Display.Buffer.Set_Source (HAL.Bitmap.Black);

   loop

      Keyboard.Scan;

      if Keyboard.Pressed (Keyboard.A1) then
         if X > Radius then
            X := X - 1;
         end if;
      end if;
      if Keyboard.Pressed (Keyboard.A2) then
         if Y > Radius then
            Y := Y - 1;
         end if;
      end if;
      if Keyboard.Pressed (Keyboard.A3) then
         if Y < Display.Height - Radius then
            Y := Y + 1;
         end if;
      end if;
      if Keyboard.Pressed (Keyboard.A4) then
         if X < Display.Width - Radius then
            X := X + 1;
         end if;
      end if;

      if Keyboard.Pressed (Keyboard.H4) then
         Radius := Radius + 1;
      end if;
      if Radius > 0 and then Keyboard.Pressed (Keyboard.H5) then
         Radius := Radius - 1;
      end if;

      if Keyboard.Pressed (Keyboard.F1) then
         Color := HAL.Bitmap.Red;
      elsif Keyboard.Pressed (Keyboard.F2) then
         Color := HAL.Bitmap.Green;
      elsif Keyboard.Pressed (Keyboard.F3) then
         Color := HAL.Bitmap.Blue;
      elsif Keyboard.Pressed (Keyboard.G1) then
         Color := HAL.Bitmap.Purple;
      elsif Keyboard.Pressed (Keyboard.G2) then
         Color := HAL.Bitmap.Pink;
      elsif Keyboard.Pressed (Keyboard.G3) then
         Color := HAL.Bitmap.Black;
      end if;

      if Keyboard.Pressed (Keyboard.C6) then
         Display.Buffer.Set_Source (HAL.Bitmap.White);
         Display.Buffer.Fill;
      end if;

      Display.Buffer.Set_Source (Color);
      Display.Buffer.Fill_Circle ((X, Y), Radius);

      if Clock > Last_FPS_Count + Seconds (1) then
         Last_FPS_Count := Clock;
         FPS := FPS_Count;
         FPS_Count := 0;
      else
         FPS_Count := FPS_Count + 1;
      end if;

      Draw_String (Buffer     => Display.Buffer.all,
                   Start      => (0, 0),
                   Msg        => "FPS: " & FPS'Img,
                   Font       => BMP_Fonts.Font12x12,
                   Foreground => HAL.Bitmap.White,
                   Background => HAL.Bitmap.Black);

      Display.Update;
   end loop;
end Main;
