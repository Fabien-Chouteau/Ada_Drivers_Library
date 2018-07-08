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

with STM32.Device;         use STM32.Device;
with STM32.GPIO;           use STM32.GPIO;
with STM32.DMA;            use STM32.DMA;
with STM32.DMA.Interrupts; use STM32.DMA.Interrupts;

with STM32_SVD.FSMC; use STM32_SVD.FSMC;
with STM32_SVD.RCC;

package body Numworks.Display is

   -- FSMC --

   SDRAM_PINS : constant GPIO_Points :=
     (PA2, PA3, PA4,
      PB12,
      PD0, PD1, PD4, PD5, PD7, PD9, PD10, PD11, PD14, PD15,
      PE10, PE11, PE12, PE13, PE14, PE15);

   Power_Pin            : GPIO_Point renames PB14;
   Reset_Pin            : GPIO_Point renames PE9;
   Extended_Command_Pin : GPIO_Point renames PB13;
   Tearing_Effect_Pin   : GPIO_Point renames PB10;

   FSMC_Base_Address : constant := 16#60000000#;
   FSMC_Memory_Bank  : constant := 1;
   FSMC_Bank_Address : constant :=
     FSMC_Base_Address + (FSMC_Memory_Bank - 1) * 16#04000000#;

   FSMC_Data_Command_Address_Bit : constant := 16;

   -- DMA --

   Screen_DMA        : STM32.DMA.DMA_Controller renames DMA_2;
   Screen_DMA_Chan   : STM32.DMA.DMA_Channel_Selector renames
     STM32.DMA.Channel_0;
   Screen_DMA_Stream : STM32.DMA.DMA_Stream_Selector renames
     STM32.DMA.Stream_0;
   Screen_DMA_Int    : STM32.DMA.Interrupts.DMA_Interrupt_Controller renames
     DMA2_Stream0;

   -- Screen interface --

   Command_Address : constant System.Address :=
     System'To_Address (FSMC_Bank_Address);

   Data_Address : constant System.Address :=
     System'To_Address
       (FSMC_Bank_Address + (2**(FSMC_Data_Command_Address_Bit + 1)));

   Command_Register : Command with Address => Command_Address;
   Data_Register    : UInt16 with Address => Data_Address, Volatile;

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Config : DMA_Stream_Configuration;
   begin
      Enable_Clock (SDRAM_PINS);

      Configure_IO (SDRAM_PINS,
                    (Mode           => Mode_AF,
                     AF             => GPIO_AF_FMC_12,
                     AF_Speed       => Speed_50MHz,
                     AF_Output_Type => Push_Pull,
                     Resistors      => Pull_Up));

      Enable_Clock (Power_Pin);

      Power_Pin.Configure_IO ((Mode         => Mode_Out,
                               Output_Type  => Push_Pull,
                               Speed        => Speed_Low,
                               Resistors    => Pull_Up));
      Power_Pin.Set;

      Enable_Clock (Reset_Pin);

      Reset_Pin.Configure_IO ((Mode         => Mode_Out,
                               Output_Type  => Push_Pull,
                               Speed        => Speed_Low,
                               Resistors    => Pull_Up));
      Reset_Pin.Set;

      Enable_Clock (Extended_Command_Pin);

      Extended_Command_Pin.Configure_IO ((Mode         => Mode_Out,
                                          Output_Type  => Push_Pull,
                                          Speed        => Speed_Low,
                                          Resistors    => Pull_Up));
      Extended_Command_Pin.Set;

      Enable_Clock (Tearing_Effect_Pin);

      Tearing_Effect_Pin.Configure_IO ((Mode      => Mode_In,
                                        Resistors => Floating));

      delay until Clock + Milliseconds (120);

      -- FSMC --

      --  See Numworks original source code more info on the values below:
      --  ion/src/device/display.cpp

      STM32_SVD.RCC.RCC_Periph.AHB3ENR.FSMCEN := True;
      STM32_SVD.RCC.RCC_Periph.AHB3RSTR.FSMCRST := True;
      STM32_SVD.RCC.RCC_Periph.AHB3RSTR.FSMCRST := False;

      FSMC_Periph.BCR1.EXTMOD := True;
      FSMC_Periph.BCR1.WREN := True;
      FSMC_Periph.BCR1.MWID := 1; -- 16 bits
      FSMC_Periph.BCR1.MTYP := 0; -- SRAM
      FSMC_Periph.BCR1.MUXEN := False;
      FSMC_Periph.BCR1.MBKEN := True;

      FSMC_Periph.BTR1.ADDSET := 2;
      FSMC_Periph.BTR1.ADDHLD := 0;
      FSMC_Periph.BTR1.DATAST := 36;
      FSMC_Periph.BTR1.BUSTURN := 10;
      FSMC_Periph.BTR1.ACCMOD := 0; -- Mode A

      FSMC_Periph.BWTR1.ADDSET := 2;
      FSMC_Periph.BWTR1.ADDHLD := 0;
      FSMC_Periph.BWTR1.DATAST := 3;
      FSMC_Periph.BWTR1.Reserved_16_19 := 3; -- BUSTURN
      FSMC_Periph.BWTR1.ACCMOD := 0; -- Mode A


      Send_Command (Reset);
      delay until Clock + Milliseconds (5);
      Send_Command (Sleep_Out);
      delay until Clock + Milliseconds (5);
      Send_Command (Pixel_Format_Set, 5);
      Send_Command (Tearing_Effect_Line_On, 0);
      Send_Command (Frame_Rate_Control, 16#1E#);
      Send_Command (Display_On, 16#1E#);

      -- DMA --

      Enable_Clock (Screen_DMA);
      Config.Channel := Screen_DMA_Chan;
      Config.Direction := Memory_To_Memory;
      Config.Increment_Peripheral_Address := True;
      Config.Increment_Memory_Address := False;
      Config.Peripheral_Data_Format := HalfWords;
      Config.Memory_Data_Format := HalfWords;
      Config.Operation_Mode := Normal_Mode;
      Config.Priority := Priority_High;
      Config.FIFO_Enabled := True;
      Config.FIFO_Threshold := FIFO_Threshold_Full_Configuration;
      Config.Memory_Burst_Size := Memory_Burst_Inc4;
      Config.Peripheral_Burst_Size := Peripheral_Burst_Inc4;
      Configure (Screen_DMA, Screen_DMA_Stream, Config);

   end Initialize;

   ------------------
   -- Send_Command --
   ------------------

   procedure Send_Command (Cmd  : Command;
                           Data : UInt8_Array := (1 .. 0 => 0))
   is
   begin
      Command_Register := Cmd;
      for Elt of Data loop
         Data_Register := UInt16 (Elt);
      end loop;
   end Send_Command;

   ------------------
   -- Send_Command --
   ------------------

   procedure Send_Command (Cmd  : Command;
                           Data : UInt8)
   is
   begin
      Send_Command (Cmd, (1 => Data));
   end Send_Command;

   ----------------------
   -- Set_Drawing_Area --
   ----------------------

   procedure Set_Drawing_Area (Area : Rect) is
      X_Start : constant UInt16 := UInt16 (Area.Position.X);
      X_End : constant UInt16 := X_Start + UInt16 (Area.Width) - 1;
      Y_Start : constant UInt16 := UInt16 (Area.Position.Y);
      Y_End : constant UInt16 := Y_Start + UInt16 (Area.Height) - 1;
   begin

      Send_Command (Memory_Access_Control, 16#A0#);
      Send_Command (Column_Address_Set,
                    (UInt8 (Shift_Right (X_Start, 8) and 16#FF#),
                     UInt8 (X_Start and 16#FF#),
                     UInt8 (Shift_Right (X_End, 8) and 16#FF#),
                     UInt8 (X_End and 16#FF#)
                    ));

      Send_Command (Page_Address_Set,
                    (UInt8 (Shift_Right (Y_Start, 8) and 16#FF#),
                     UInt8 (Y_Start and 16#FF#),
                     UInt8 (Shift_Right (Y_End, 8) and 16#FF#),
                     UInt8 (Y_End and 16#FF#)
                    ));
   end Set_Drawing_Area;

   -----------------------
   -- Start_Pixel_Write --
   -----------------------

   procedure Start_Pixel_Write is
   begin
      Send_Command (Memory_Write);
   end Start_Pixel_Write;

   ----------------
   -- Push_Pixel --
   ----------------

   procedure Push_Pixel (Pixel : HAL.UInt16) is
   begin
      Data_Register := Pixel;
   end Push_Pixel;

   -----------------
   -- Push_Pixels --
   -----------------

   procedure Push_Pixels (Pixels       : HAL.UInt16_Array;
                          DMA_Theshold : Natural := 256)
   is
   begin
      if Pixels'Length <= DMA_Theshold then
         for Pixel of Pixels loop
            Data_Register := Pixel;
         end loop;
      else
         Wait_DMA_Transfer;
         Start_DMA_Transfer (Pixels'Address, Pixels'Length);
      end if;
   end Push_Pixels;

   ----------------------
   -- Wait_End_Of_Push --
   ----------------------

   procedure Wait_End_Of_Push is
   begin
      Wait_DMA_Transfer;
   end Wait_End_Of_Push;

   ------------------------
   -- Start_DMA_Transfer --
   ------------------------

   procedure Start_DMA_Transfer (Addr  : System.Address;
                                 Count : UInt16)
   is
   begin
      Screen_DMA_Int.Start_Transfer
        (Source      => Addr,
         Destination => Data_Register'Address,
         Data_Count  => Count);
   end Start_DMA_Transfer;

   -----------------------
   -- Wait_DMA_Transfer --
   -----------------------

   procedure Wait_DMA_Transfer is
      Status : DMA_Error_Code;
   begin
      Screen_DMA_Int.Wait_For_Completion (Status);

      if Status /= DMA_No_Error then
         if Status = DMA_Timeout_Error then
            raise Program_Error with "DMA timeout! Transferred: " &
              Items_Transferred (Screen_DMA, Screen_DMA_Stream)'Img;
         else
            raise Program_Error;
         end if;
      end if;
   end Wait_DMA_Transfer;

begin
   Initialize;
end Numworks.Display;
