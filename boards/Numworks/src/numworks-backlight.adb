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

with Ada.Real_Time; use Ada.Real_Time;
with HAL;           use HAL;
with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;

package body Numworks.Backlight is

   Enable_Pin : GPIO_Point renames PC6;

   sLevel : Level := 16;

   procedure Initialize;
   procedure Send_Pulses (Pulses : UInt8);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Enable_Clock (Enable_Pin);

      Enable_Pin.Configure_IO ((Mode         => Mode_Out,
                                Output_Type  => Push_Pull,
                                Speed        => Speed_Low,
                                Resistors    => Pull_Up));
      Resume;
   end Initialize;

   -----------------
   -- Send_Pulses --
   -----------------

   procedure Send_Pulses (Pulses : UInt8) is
   begin
      for X in 1 .. Pulses loop
         Enable_Pin.Clear;
         delay until Clock + Milliseconds (1);
         Enable_Pin.Set;
         delay until Clock + Milliseconds (1);
      end loop;
   end Send_Pulses;

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (Lvl : Level) is
   begin
      if sLevel < Lvl then
         Send_Pulses (16 + UInt8 (sLevel) - UInt8 (Lvl));
      else
         Send_Pulses (UInt8 (sLevel) - UInt8 (Lvl));
      end if;
      sLevel := Lvl;
   end Set_Level;

   ------------
   -- Resume --
   ------------

   procedure Resume is
      Lvl : constant Level := sLevel;
   begin
      Enable_Pin.Set;
      delay until Clock + Milliseconds (1);
      sLevel := 16;
      Set_Level (Lvl);
   end Resume;

begin
   Initialize;
end Numworks.Backlight;
