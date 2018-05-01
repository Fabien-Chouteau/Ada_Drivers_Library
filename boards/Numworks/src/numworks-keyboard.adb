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

with STM32.Device; use STM32.Device;
with STM32.GPIO;   use STM32.GPIO;

package body Numworks.Keyboard is

   type State_Array is array (Key) of Boolean with Pack;

   State : State_Array := (others => False);

   subtype Row_Index is Integer range  1 .. 9;
   subtype Column_Index is Integer range 1 .. 6;

   Row_Points : GPIO_Points (Row_Index) :=
     (PE0, PE1, PE2, PE3, PE4, PE5, PE6, PE7, PE8);

   Column_Points : constant GPIO_Points (Column_Index) :=
     (PC0, PC1, PC2, PC3, PC4, PC5);

   Keymap : constant array (Row_Index, Column_Index) of Key :=
     ((A1, A2, A3, A4, A5, A6),
      (B1, B2, Na, Na, Na, Na),
      (C1, C2, C3, C4, C5, C6),
      (D1, D2, D3, D4, D5, D6),
      (E1, E2, E3, E4, E5, E6),
      (F1, F2, F3, F4, F5, Na),
      (G1, G2, G3, G4, G5, Na),
      (H1, H2, H3, H4, H5, Na),
      (I1, I2, I3, I4, I5, Na));

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Enable_Clock (Row_Points);

      Configure_IO (Row_Points,
                    (Mode         => Mode_Out,
                     Output_Type  => Open_Drain,
                     Speed        => Speed_Low,
                     Resistors    => Floating));

      --  Deactivate all rows
      for Row of Row_Points loop
         Row.Set;
      end loop;

      Enable_Clock (Column_Points);
      Configure_IO (Column_Points,
                    (Mode      => Mode_In,
                     Resistors => Pull_Up));

   end Initialize;

   ----------
   -- Scan --
   ----------

   procedure Scan is
   begin
      for Row in Row_Index loop
         Row_Points (Row).Clear; -- Activate row
         for Column in Column_Index loop
            State (Keymap (Row, Column)) := not Column_Points (Column).Set;
         end loop;
         Row_Points (Row).Set; -- Deactivate row
      end loop;
   end Scan;

   -------------
   -- Pressed --
   -------------

   function Pressed (K : Key) return Boolean
   is (State (K));

begin
   Initialize;
end Numworks.Keyboard;
