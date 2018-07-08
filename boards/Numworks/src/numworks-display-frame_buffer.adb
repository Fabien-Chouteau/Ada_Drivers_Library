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

with HAL.Bitmap;

package body Numworks.Display.Frame_Buffer is

   Buffer_Data : UInt16_Array (0 .. Width * Height - 1);

   Frame_Buffer : aliased Memory_Mapped_Bitmap_Buffer :=
     (Addr              => Buffer_Data'Address,
      Actual_Width      => 320,
      Actual_Height     => 240,
      Actual_Color_Mode => HAL.Bitmap.RGB_565,
      Currently_Swapped => False,
      Native_Source     => 0);

   ------------
   -- Buffer --
   ------------

   function Buffer return not null Any_Memory_Mapped_Bitmap_Buffer
   is (Frame_Buffer'Access);

   ------------
   -- Update --
   ------------

   procedure Update is
      Index : Integer;
      Count : UInt16;
   begin

      --  Parameters of the first transfer
      Count := UInt16'Last;
      Index := 0;

      Send_Command (Memory_Write);

      Start_DMA_Transfer (Buffer_Data (Index)'Address, Count);
      Wait_DMA_Transfer;

      --  Parameters of the second transfer
      Count := (Width * Height) - UInt16'Last;
      Index := Integer (UInt16'Last);

      Start_DMA_Transfer (Buffer_Data (Index)'Address, Count);
      Wait_DMA_Transfer;
   end Update;

end Numworks.Display.Frame_Buffer;
