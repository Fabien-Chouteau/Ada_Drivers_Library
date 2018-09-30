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
with Ada.Text_IO;

package body DWC_OTG_FS is

   Rx_FIFO_Size : constant := 64;

   Verbose : constant Boolean := False;
   procedure Put_Line (Str : String);
   procedure Put_Line (Str : String) is
   begin
      if Verbose then
         Ada.Text_IO.Put_Line (Str);
      end if;
   end Put_Line;

   function Valid (EP : EP_Id) return Boolean;
   procedure Flush_RX_FIFO;
   procedure Flush_TX_FIFO;
   procedure Disable_Interrupt;
   procedure Enable_Interrupt;
   procedure USB_Disconnect;
   procedure USB_Connect;
   procedure Set_RX_Fifo (Size : UInt16);
   procedure Set_TX_Fifo (This : in out OTG_USB_Device;
                          Ep   : EP_Id;
                          Size : UInt16);
   procedure EP0_Out_Start (This : in out OTG_USB_Device);

   -----------
   -- Valid --
   -----------

   function Valid (EP : EP_Id) return Boolean
   is (EP <= EP_Id (All_EP_Index'Last));

   -------------------
   -- Flush_RX_FIFO --
   -------------------

   procedure Flush_RX_FIFO is
   begin
      GLOBAL_Periph.GRSTCTL.RXFFLSH := True;
      GLOBAL_Periph.GRSTCTL.RXFFLSH := True;

      while GLOBAL_Periph.GRSTCTL.RXFFLSH loop
         null;
      end loop;
   end Flush_RX_FIFO;

   -------------------
   -- Flush_TX_FIFO --
   -------------------

   procedure Flush_TX_FIFO is
   begin

      --  Flush all FIFO
      GLOBAL_Periph.GRSTCTL.TXFNUM := 2#10000#;

      GLOBAL_Periph.GRSTCTL.TXFFLSH := True;

      while GLOBAL_Periph.GRSTCTL.TXFFLSH loop
         null;
      end loop;
   end Flush_TX_FIFO;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt is
   begin
      GLOBAL_Periph.GAHBCFG.GINT := False;
   end Disable_Interrupt;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt is
   begin
      GLOBAL_Periph.GAHBCFG.GINT := True;
   end Enable_Interrupt;

   --------------------
   -- USB_Disconnect --
   --------------------

   procedure USB_Disconnect is
   begin
      DEVICE_Periph.DCTL.SDIS := True;
   end USB_Disconnect;

   -----------------
   -- USB_Connect --
   -----------------

   procedure USB_Connect is
   begin
      DEVICE_Periph.DCTL.SDIS := False;
   end USB_Connect;

   -----------------
   -- Set_RX_Fifo --
   -----------------

   procedure Set_RX_Fifo (Size : UInt16) is
   begin
      GLOBAL_Periph.GRXFSIZ.RXFD := Size;
   end Set_RX_Fifo;

   -----------------
   -- Set_TX_Fifo --
   -----------------

   procedure Set_TX_Fifo (This : in out OTG_USB_Device;
                          Ep   : EP_Id;
                          Size : UInt16) is
      pragma Unreferenced (This);
      TX_Offset : UInt16 := GLOBAL_Periph.GRXFSIZ.RXFD;
   begin

      if not Valid (Ep) then
         raise Program_Error with "invalid EP in Set_TX_Fifo";
      end if;

      if Ep = 0 then
         GLOBAL_Periph.HPTXFSIZ.PTXFD := Size;
         GLOBAL_Periph.HPTXFSIZ.PTXSA := TX_Offset;
      else
         TX_Offset := TX_Offset + GLOBAL_Periph.HPTXFSIZ.PTXFD;

         for Index in TX_Fifo_Index loop
            if TX_Fifo_Index (All_EP_Index (Ep)) >= Index then
               TX_Offset := TX_Offset + GLOBAL_Periph.DIEPTXF (Index).INEPTXFD;
            else
               exit;
            end if;
         end loop;

         GLOBAL_Periph.DIEPTXF (TX_Fifo_Index (Ep)).INEPTXSA := TX_Offset;
         GLOBAL_Periph.DIEPTXF (TX_Fifo_Index (Ep)).INEPTXFD := Size;
      end if;
   end Set_TX_Fifo;

   -------------------
   -- EP0_Out_Start --
   -------------------

   procedure EP0_Out_Start (This : in out OTG_USB_Device) is
   begin
      DEVICE_Periph.DOEPTSIZ0 := (STUPCNT => 3,
                                  PKTCNT  => True,
                                  XFRSIZ  => 3 * 8,
                                  others  => <>);
      if This.Force_NAK (0) then
         DEVICE_Periph.DOEPCTL0.SNAK := True;
      else
         DEVICE_Periph.DOEPCTL0.CNAK := True;
      end if;
      DEVICE_Periph.DOEPCTL0.EPENA := True;
   end EP0_Out_Start;

   -----------
   -- Start --
   -----------

   overriding
   procedure Start (This : in out OTG_USB_Device) is
      pragma Unreferenced (This);
   begin
      USB_Connect;
      Enable_Interrupt;
   end Start;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (This : in out OTG_USB_Device) is
   begin

      Disable_Interrupt;

      --  Using embedded PHY
      GLOBAL_Periph.GUSBCFG.PHYSEL := True;

      --  Wait for AHB idle
      while not GLOBAL_Periph.GRSTCTL.AHBIDL loop
         null;
      end loop;

      --  Do core soft reset
      GLOBAL_Periph.GRSTCTL.CSRST := True;
      while GLOBAL_Periph.GRSTCTL.CSRST loop
         null;
      end loop;

      --  Enable VBUS sensing in device mode and power up the PHY
      GLOBAL_Periph.GCCFG := (PWRDWN => True,
                              others => <>);
      --  End of core init

      --  Clear mode
      GLOBAL_Periph.GUSBCFG.FHMOD := False;
      GLOBAL_Periph.GUSBCFG.FDMOD := False;

      --  Set device mode
      GLOBAL_Periph.GUSBCFG.FDMOD := True;

      delay until Clock + Milliseconds (50);

      --  USB_DevInit...

      --  Enable VBUS sensing

      --  VBus sensing depends on the version of the OTG controller
      if GLOBAL_Periph.CID < 16#2000# then

         GLOBAL_Periph.GCCFG.VBUSBSEN := True;

         --  FIXME: User should be able to disable VBUS sensing (f40x era)
      else

         --  This version of the device has a different layout for the GCCFG
         --  register. It happens that NOVBUSSENS on the old device is the
         --  same as VBDEN on the newer device.

         --  FIXME: Have a correct layout for this version
         GLOBAL_Periph.GCCFG.NOVBUSSENS := True;

         --  FIXME: User should be able to disable VBUS sensing (f446 era)
      end if;

      --  Restart the Phy Clock
      PWRCLK_Periph.PCGCR := (STPPCLK  => False,
                              GATEHCLK => False,
                              PHYSUSP  => False,
                              others   => <>);

      --  Device mode configuration
      DEVICE_Periph.DCFG.PFIVL := 0; -- 80% frame interval

      DEVICE_Periph.DCFG.DSPD := 3; -- Set full speed

      Flush_RX_FIFO;
      Flush_TX_FIFO;

      --  Clear all device interrupts
      DEVICE_Periph.DIEPMSK :=
        (XFRCM     => False,
         EPDM      => False,
         TOM       => False,
         ITTXFEMSK => False,
         INEPNMM   => False,
         INEPNEM   => False,
         others    => <>);
      DEVICE_Periph.DOEPMSK :=
        (XFRCM  => False,
         EPDM   => False,
         STUPM  => False,
         OTEPDM => False,
         others    => <>);
      DEVICE_Periph.DAINT := (16#FFFF#, 16#FFFF#);
      DEVICE_Periph.DAINTMSK := (0, 0);

      if DEVICE_Periph.DIEPCTL0.EPENA then
         DEVICE_Periph.DIEPCTL0 := (EPDIS  => True,
                                              SNAK   => True,
                                              others => <>);
      else
         DEVICE_Periph.DIEPCTL0 := (others => <>);
      end if;
      DEVICE_Periph.DIEPTSIZ0 := (others => <>);
      DEVICE_Periph.DIEPINT0 :=
        (XFRC   => True,
         EPDISD => True,
         TOC    => True,
         ITTXFE => True,
         INEPNE => True,
         TXFE   => True,
         others => <>);

      -- In End Points --
      for Ep in EP_Index loop
         if DEVICE_Periph.IEP (Ep).CTL.EPENA then
            DEVICE_Periph.IEP (Ep).CTL := (EPDIS  => True,
                                           SNAK   => True,
                                           others => <>);
         else
            DEVICE_Periph.IEP (Ep).CTL := (others => <>);
         end if;
         DEVICE_Periph.IEP (Ep).SIZ := (others => <>);
         DEVICE_Periph.IEP (Ep).INT :=
           (XFRC   => True,
            EPDISD => True,
            TOC    => True,
            ITTXFE => True,
            INEPNE => True,
            TXFE   => True,
            others => <>);
      end loop;

      if DEVICE_Periph.DOEPCTL0.EPENA then
         DEVICE_Periph.DOEPCTL0 := (EPDIS  => True,
                                              SNAK   => True,
                                              others => <>);
      else
         DEVICE_Periph.DOEPCTL0 := (others => <>);
      end if;

      DEVICE_Periph.DOEPTSIZ0 := (others => <>);
      DEVICE_Periph.DOEPINT0 :=
        (XFRC   => True,
         EPDISD => True,
         STUP    => True,
         OTEPDIS => True,
         B2BSTUP => True,
         others => <>);

      -- Out End Points --
      for Ep in EP_Index loop

         if DEVICE_Periph.OEP (Ep).CTL.EPENA then
            DEVICE_Periph.OEP (Ep).CTL := (EPDIS  => True,
                                           SNAK   => True,
                                           others => <>);
         else
            DEVICE_Periph.OEP (Ep).CTL := (others => <>);
         end if;
         DEVICE_Periph.OEP (Ep).SIZ := (others => <>);
         DEVICE_Periph.OEP (Ep).INT :=
           (XFRC   => True,
            EPDISD => True,
            STUP    => True,
            OTEPDIS => True,
            B2BSTUP => True,
            others => <>);
      end loop;

      --  FIXME? Should we clear FIFO underrun? But it doesn't exist in the SVD
      --  DEVICE_Periph.DIEPMSK.

      --  Disable all interrupts
      GLOBAL_Periph.GINTMSK := (others => <>);
      --  Clear all interrupts
      GLOBAL_Periph.GINTSTS :=
        (
         CMOD               => True,
         MMIS               => True,
         OTGINT             => True,
         SOF                => True,
         RXFLVL             => True,
         NPTXFE             => True,
         GINAKEFF           => True,
         GOUTNAKEFF         => True,
         ESUSP              => True,
         USBSUSP            => True,
         USBRST             => True,
         ENUMDNE            => True,
         ISOODRP            => True,
         EOPF               => True,
         IEPINT             => True,
         OEPINT             => True,
         IISOIXFR           => True,
         PXFR_INCOMPISOOUT  => True,
         HPRTINT            => True,
         HCINT              => True,
         PTXFE              => True,
         CIDSCHG            => True,
         DISCINT            => True,
         SRQINT             => True,
         WKUPINT            => True,
         others => <>);

      --  if DMA disabled
      GLOBAL_Periph.GINTMSK.RXFLVLM := True;

      --  Common interrupt
      GLOBAL_Periph.GINTMSK.USBSUSPM := True;
      GLOBAL_Periph.GINTMSK.USBRST := True;
      GLOBAL_Periph.GINTMSK.ENUMDNEM := True;
      GLOBAL_Periph.GINTMSK.IEPINT := True;
      GLOBAL_Periph.GINTMSK.OEPINT := True;
      GLOBAL_Periph.GINTMSK.IISOIXFRM := True;
      GLOBAL_Periph.GINTMSK.PXFRM_IISOOXFRM := True;
      GLOBAL_Periph.GINTMSK.WUIM := True;

      --  VBUS sensing interrupt
      GLOBAL_Periph.GINTMSK.SRQIM := True;
      GLOBAL_Periph.GINTMSK.OTGINT := True;

      USB_Disconnect;

      Set_RX_Fifo (16#80#);
      Set_TX_Fifo (This, 0, 16#40#);
      Set_TX_Fifo (This, 1, 16#80#);

      Put_Line ("Init finished");

   end Initialize;

   ---------------------
   -- Set_EP_Callback --
   ---------------------

   overriding
   procedure Set_EP_Callback (This     : in out OTG_USB_Device;
                              EP       : EP_Addr;
                              Callback : EP_Callback)

   is
   begin
      This.EP_Callbacks (EP.Num, EP.Dir) := Callback;
   end Set_EP_Callback;

   ------------------------
   -- Set_Setup_Callback --
   ------------------------

   overriding
   procedure Set_Setup_Callback (This     : in out OTG_USB_Device;
                                 EP       : EP_Id;
                                 Callback : Setup_Callback)
   is
   begin
      This.Setup_Callbacks (EP) := Callback;
   end Set_Setup_Callback;

   --------------
   -- EP_Setup --
   --------------

   overriding
   procedure EP_Setup (This     : in out OTG_USB_Device;
                       EP       : EP_Addr;
                       Typ      : EP_Type;
                       Max_Size : UInt16;
                       Callback : EP_Callback)
   is
      MPSIZ : UInt2;
   begin

      if EP.Num = 0 then
         --  Control endpoint

         --  Input
         if Max_Size >= 64 then
            MPSIZ := 0;
         elsif Max_Size >= 32 then
            MPSIZ := 1;
         elsif Max_Size >= 16 then
            MPSIZ := 2;
         else
            MPSIZ := 3;
         end if;

         DEVICE_Periph.DIEPTSIZ0.XFRSIZ :=
           DIEPTSIZ0_XFRSIZ_Field (Max_Size);
         DEVICE_Periph.DIEPTSIZ0.PKTCNT := 0;
         DEVICE_Periph.DIEPCTL0 :=
           (
            MPSIZ          => UInt11 (MPSIZ),
            USBAEP         => True,
            NAKSTS         => False,
            EPTYP          => 0, -- hardcoded 0 for control
            Stall          => False,
            TXFNUM         => 0,
            CNAK           => False,
            SNAK           => True,
            EPDIS          => False,
            EPENA          => True,
            others => <>);
         DEVICE_Periph.DIEPINT0 :=
           (XFRC   => True,
            EPDISD => True,
            TOC    => True,
            ITTXFE => True,
            INEPNE => True,
            TXFE   => True,
            others => <>);

         --  Output
         DEVICE_Periph.DOEPTSIZ0 := (STUPCNT => 3,
                                     PKTCNT  => True,
                                     XFRSIZ  => 3 * 8,
                                     others  => <>);
         DEVICE_Periph.DOEPCTL0 :=
           (
            MPSIZ          => MPSIZ,
            USBAEP         => True,
            NAKSTS         => False,
            EPTYP          => 0, -- hardcoded 0 for control
            SNPM           => False,
            Stall          => False,
            CNAK           => False,
            SNAK           => True,
            EPDIS          => False,
            EPENA          => True,
            others => <>);

         --  FIFO
         GLOBAL_Periph.GNPTXFSIZ_Host.NPTXFD := Max_Size / 4;
         GLOBAL_Periph.GNPTXFSIZ_Host.NPTXFSA := Rx_FIFO_Size;

         DEVICE_Periph.DOEPINT0 :=
           (XFRC    => True,
            EPDISD  => True,
            STUP    => True,
            OTEPDIS => True,
            B2BSTUP => True,
            others  => <>);

         DEVICE_Periph.DAINTMSK.IEPM := DEVICE_Periph.DAINTMSK.IEPM or 1;
         DEVICE_Periph.DAINTMSK.OEPM := DEVICE_Periph.DAINTMSK.OEPM or 1;

         return;
      end if;

      if EP.Num not in EP_Id (EP_Index'First) .. EP_Id (EP_Index'Last) then
         raise Program_Error with "invalid EP number";
      end if;

      case EP.Dir is
         when EP_In =>
            Set_TX_Fifo (This, EP.Num, Max_Size);

            DEVICE_Periph.IEP (EP_Index (EP.Num)).SIZ.XFRSIZ := UInt19 (Max_Size);
            DEVICE_Periph.IEP (EP_Index (EP.Num)).CTL :=
              (
               MPSIZ          => UInt11 (Max_Size),
               USBAEP         => True,
               EONUM_DPID     => False,
               NAKSTS         => False,
               EPTYP          => EP_Type'Enum_Rep (Typ),
               Stall          => False,
               TXFNUM         => EP.Num,
               CNAK           => False,
               SNAK           => True,

               --  USB data packet have either a 0 or 1 id alternatively. This
               --  bit should not be set when restarting and EP, otherwise the
               --  controller will reject every other packet.
               --
               --  FIXME: How to set this bit only when configuring an EP and
               --  not when restarting it? Maybe with a dedicated sub-program
               --  to restart en EP.
               SD0PID_SEVNFRM => False,

               SODDFRM        => False,
               EPDIS          => False,
               EPENA          => True,
               others => <>
              );

            This.EP_Callbacks (EP.Num, EP_In) := Callback;
         when EP_Out =>
            This.EP_Callbacks (EP.Num, EP_Out) := Callback;

            DEVICE_Periph.OEP (EP_Index (EP.Num)).SIZ.XFRSIZ := UInt19 (Max_Size);
            DEVICE_Periph.OEP (EP_Index (EP.Num)).SIZ.PKTCNT := 1;
            DEVICE_Periph.OEP (EP_Index (EP.Num)).CTL :=
              (
               MPSIZ          => UInt11 (Max_Size),
               USBAEP         => True,
               EONUM_DPID     => False,
               NAKSTS         => False,
               EPTYP          => EP_Type'Enum_Rep (Typ),
               Stall          => False,
               CNAK           => True,
               SNAK           => False,

               --  See above
               SD0PID_SEVNFRM => False,

               SODDFRM        => False,
               EPDIS          => False,
               EPENA          => True,
               others => <>
              );
      end case;
   end EP_Setup;

   ----------------
   -- EP_Set_NAK --
   ----------------

   overriding
   procedure EP_Set_NAK (This : in out OTG_USB_Device;
                         EP   : EP_Addr;
                         NAK  : Boolean)
   is
   begin
      if EP.Dir /= EP_Out then
         return;
      end if;

      This.Force_NAK (EP.Num) := NAK;

      if EP.Num > EP_Id (All_EP_Index'Last) then
         raise Program_Error with "invalid EP number in set NAK";
      end if;

      if NAK then
         case EP.Num is
            when 0 => DEVICE_Periph.DOEPCTL0.SNAK := True;
            when others =>
               DEVICE_Periph.OEP (EP_Index (EP.Num)).CTL.SNAK := True;
         end case;
      else
         case EP.Num is
            when 0 => DEVICE_Periph.DOEPCTL0.CNAK := True;
            when others =>
               DEVICE_Periph.OEP (EP_Index (EP.Num)).CTL.CNAK := True;
         end case;
      end if;

   end EP_Set_NAK;

   ------------------
   -- EP_Set_Stall --
   ------------------

   overriding
   procedure EP_Set_Stall (This : in out OTG_USB_Device;
                           EP   : EP_Addr)
   is
   begin

      if EP.Num > EP_Id (All_EP_Index'Last) then
         raise Program_Error with "invalid EP number in set stall";
      end if;

      case EP.Dir is
         when EP_In =>
            case EP.Num is
               when 0 => DEVICE_Periph.DIEPCTL0.Stall := True;
               when others =>
                  DEVICE_Periph.IEP (EP_Index (EP.Num)).CTL.Stall := True;
            end case;
         when EP_Out =>
            case EP.Num is
               when 0 =>
                  DEVICE_Periph.DOEPCTL0.Stall := True;
                  --  We still want to be able to receive setup packets
                  EP0_Out_Start (This);
               when others =>
                  DEVICE_Periph.OEP (EP_Index (EP.Num)).CTL.Stall := True;
            end case;
      end case;
   end EP_Set_Stall;

   -----------------
   -- Set_Address --
   -----------------

   overriding
   procedure Set_Address (This : in out OTG_USB_Device;
                          Addr : UInt7)
   is
      pragma Unreferenced (This);
   begin
      DEVICE_Periph.DCFG.DAD := 0;
      DEVICE_Periph.DCFG.DAD := Addr;
   end Set_Address;

   --------------------
   -- EP_Read_Packet --
   --------------------

   overriding
   procedure EP_Read_Packet (This : in out OTG_USB_Device;
                             EP   : EP_Id;
                             Addr : System.Address;
                             Len  : UInt32)
   is
      pragma Unreferenced (EP);

      --        Cnt_8  : UInt32 := UInt32'Min (Len, UInt32 (This.RX_BCNT));
      Cnt_8 : UInt32 := Len;
      Cnt_32 : constant UInt32 := (Cnt_8 + 3) / 4;

      Buf_32  : UInt32_Array (1 .. Integer (Cnt_32)) with Address => Addr;
      Buf_8  : UInt8_Array (1 .. Integer (Cnt_8)) with Address => Addr;

      Data : UInt32;
   begin

      for Index in Buf_32'Range loop
         Data := RX_FIFO;
         Buf_32 (Index) := Data;
         if Verbose then
            Put_Line ("Cnt:" & Cnt_8'Img);
            Put_Line ("FIFO:" & Data'Img);
         end if;
         This.RX_BCNT := This.RX_BCNT - 4;
         Cnt_8 := Cnt_8 - 4;
      end loop;

      if Cnt_8 > 0 then
         raise Program_Error with "Not implemented";
--           Last := FIFO;
--           Put_Line ("Cnt:" & Cnt'Img);
--           Put_Line ("Last:" & Last'Img);
      end if;

   end EP_Read_Packet;

   ---------------------
   -- EP_Write_Packet --
   ---------------------

   overriding
   procedure EP_Write_Packet (This : in out OTG_USB_Device;
                              Ep   : EP_Id;
                              Addr : System.Address;
                              Len  : UInt32)
   is
      pragma Unreferenced (This);
      Cnt_8  : UInt32 := Len;
      Cnt_32 : constant UInt32 := (Cnt_8 + 3) / 4;

      Buf_8  : UInt8_Array (1 .. Integer (Cnt_8)) with Address => Addr;
      Buf_32  : UInt32_Array (1 .. Integer (Cnt_32)) with Address => Addr;
   begin

      if Len > UInt32 (UInt7'Last) then
         raise Program_Error with "invalid size";
      end if;

      if Ep = 0 then

         DEVICE_Periph.DIEPTSIZ0.XFRSIZ := UInt7 (Len);
         DEVICE_Periph.DIEPTSIZ0.PKTCNT := 1;
         DEVICE_Periph.DIEPCTL0.CNAK := True;
         DEVICE_Periph.DIEPCTL0.EPENA := True;

      elsif Valid (Ep) then

         DEVICE_Periph.IEP (EP_Index (Ep)).SIZ.XFRSIZ := UInt19 (Len);
         DEVICE_Periph.IEP (EP_Index (Ep)).SIZ.PKTCNT := 1;
         DEVICE_Periph.IEP (EP_Index (Ep)).SIZ.MCNT := 1; -- FIXME: Is that always a good value?

         DEVICE_Periph.IEP (EP_Index (Ep)).CTL.CNAK := True;
         DEVICE_Periph.IEP (EP_Index (Ep)).CTL.EPENA := True;

      else
         raise Program_Error with "Invalid EP in write packet";
      end if;

      declare
         TX_FIFO : aliased UInt32
           with Size => 32,
           Volatile_Full_Access,
           Address =>
             System'To_Address (Base_Address + 16#1000# + 16#1000# * UInt32 (Ep));
      begin
         for Elt of Buf_32 loop
            TX_FIFO := Elt;
            Cnt_8 := Cnt_8 - 4;
         end loop;
      end;

   end EP_Write_Packet;

   ----------
   -- Poll --
   ----------

   overriding
   function Poll (This : in out OTG_USB_Device) return UDC_Event is
      GINTSTS : UInt32 with Volatile_Full_Access,
        Address => GLOBAL_Periph.GINTSTS'Address;
   begin

      --  It is important to check the interrupt flag in this order
      --  In particular, reset should be checked last to make sure the transfer
      --  completions are processed before doing a reset.

      Put_Line ("GINTSTS:" & GINTSTS'Img);

      if GLOBAL_Periph.GINTSTS.OEPINT then
         Put_Line ("GINTSTS.OEPINT");
         GLOBAL_Periph.GINTSTS.OEPINT := True;

         declare
            EP : EP_Id := 0;
            OEPINT : UInt16 :=  DEVICE_Periph.DAINT.OEPINT;

         begin

            while OEPINT /= 0 loop
               if (OEPINT and 1) /= 0 then

                  if EP = 0 then
                     if DEVICE_Periph.DOEPINT0.STUP then
                        DEVICE_Periph.DOEPINT0.STUP := True;
                        Put_Line ("DOEPINT0.STUP");

                        --  Prepare EP0 for the next setup
                        EP0_Out_Start (This);
                        return (Kind   => Setup_Request,
                                Req    => This.Setup_Req,
                                Req_EP => EP);
                     end if;
                     if DEVICE_Periph.DOEPINT0.XFRC then
                        DEVICE_Periph.DOEPINT0.XFRC := True;
                        Put_Line ("DOEPINT0.XFRC");
                     end if;
                     if DEVICE_Periph.DOEPINT0.EPDISD then
                        DEVICE_Periph.DOEPINT0.EPDISD := True;
                        Put_Line ("DOEPINT0.EPDISD");
                     end if;
                     if DEVICE_Periph.DOEPINT0.OTEPDIS then
                        DEVICE_Periph.DOEPINT0.OTEPDIS := True;
                        Put_Line ("DOEPINT0.OTEPDIS");
                     end if;
                     if DEVICE_Periph.DOEPINT0.B2BSTUP then
                        DEVICE_Periph.DOEPINT0.B2BSTUP := True;
                        Put_Line ("DOEPINT0.B2BSTUP");
                     end if;
                  else
                     if DEVICE_Periph.OEP (EP_Index (EP)).INT.XFRC then
                        DEVICE_Periph.OEP (EP_Index (EP)).INT.XFRC := True;
                        return (Kind  => Transfer_Complete,
                                T_EP => (EP, EP_Out));
                     end if;
                     if DEVICE_Periph.OEP (EP_Index (EP)).INT.NYET then
                        DEVICE_Periph.OEP (EP_Index (EP)).INT.NYET := True;
                        Put_Line ("DOEPINTX.NYET");
                     end if;
                     if DEVICE_Periph.OEP (EP_Index (EP)).INT.EPDISD then
                        DEVICE_Periph.OEP (EP_Index (EP)).INT.EPDISD := True;
                        Put_Line ("DOEPINTX.EPDISD");
                     end if;
                     if DEVICE_Periph.OEP (EP_Index (EP)).INT.OTEPDIS then
                        DEVICE_Periph.OEP (EP_Index (EP)).INT.OTEPDIS := True;
                        Put_Line ("DOEPINTX.OTEPDIS");
                     end if;
                  end if;
               end if;
               OEPINT := Shift_Right (OEPINT, 1);
               EP := EP + 1;
            end loop;
         end;
      end if;

      if GLOBAL_Periph.GINTSTS.IEPINT then

         Put_Line ("GINTSTS.IEPINT");
         GLOBAL_Periph.GINTSTS.IEPINT := True;
         declare
            EP : EP_Id := 0;
            IEPINT :  UInt16 :=  DEVICE_Periph.DAINT.IEPINT;

         begin

            while IEPINT /= 0 loop
               if (IEPINT and 1) /= 0 then

                  case EP is

                     when 0 =>
                        if DEVICE_Periph.DIEPINT0.XFRC then
                           DEVICE_Periph.DIEPINT0.XFRC := True;
                           Put_Line ("DIEPINT0");

                           return (Kind => Transfer_Complete, T_EP => (0, EP_In));
                        end if;
                     when EP_Id (EP_Index'First) .. EP_Id (EP_Index'Last) =>
                        if DEVICE_Periph.IEP (EP_Index (EP)).INT.XFRC then
                           DEVICE_Periph.IEP (EP_Index (EP)).INT.XFRC := True;

                           if Verbose then
                              Put_Line ("DIEPINT" & EP'Img);
                           end if;

                           return (Kind => Transfer_Complete, T_EP => (EP, EP_In));
                        end if;
                     when others =>
                        raise Program_Error with "IEP doesn't exists";
                  end case;
               end if;
               IEPINT := Shift_Right (IEPINT, 1);
               EP := EP + 1;
            end loop;
         end;
      end if;

      if GLOBAL_Periph.GINTSTS.USBSUSP then
         GLOBAL_Periph.GINTSTS.USBSUSP := True;
         Put_Line ("GINTSTS.USBSUSP");
      end if;
      if GLOBAL_Periph.GINTSTS.WKUPINT then
         GLOBAL_Periph.GINTSTS.WKUPINT := True;
         Put_Line ("GINTSTS.WLUPINT");
      end if;
      if GLOBAL_Periph.GINTSTS.SOF then
         GLOBAL_Periph.GINTSTS.SOF := True;
         Put_Line ("GINTSTS.SOF");
      end if;

      if GLOBAL_Periph.GINTSTS.RXFLVL then

         --  This interrupt is not cleared by setting the flag to 1???
         --  GLOBAL_Periph.GINTSTS.RXFLVL := True;

         Put_Line ("GINTSTS.RXFLVL");

         declare
            --  PKTSTS_GOUTNAK    : constant := 1;
            PKTSTS_OUT        : constant := 2;
            --  PKTSTS_OUT_COMP   : constant := 3;
            --  PKTSTS_SETUP_COMP : constant := 4;
            PKTSTS_SETUP      : constant := 6;

            RX_Status : constant GRXSTSP_Peripheral_Register
              := GLOBAL_Periph.GRXSTSP_Peripheral;
            --  It is very important to use the pop version of this register,
            --  otherwise the control data stays in the FIFO and will be mixed
            --  with the real data.

         begin
--              Put_Line ("DIEPTSIZ0.PKTCNT:" &
--                          DEVICE_Periph.DIEPTSIZ0.PKTCNT'Img);

            if Verbose then
               Put_Line ("PKTSTS:" & RX_Status.PKTSTS'Img);
               Put_Line ("BCNT:" & RX_Status.BCNT'Img);
            end if;

            if RX_Status.PKTSTS = PKTSTS_OUT then

               Put_Line ("PKTSTS_OUT");
               return (Kind    => Data_Ready,
                       RX_EP   => RX_Status.EPNUM,
                       RX_BCNT => RX_Status.BCNT);
            elsif RX_Status.PKTSTS = PKTSTS_SETUP then

               --  Receive a setup packet
               This.EP_Read_Packet (RX_Status.EPNUM,
                                    This.Setup_Req'Address,
                                    8);
            end if;

         end;
      end if;

      if GLOBAL_Periph.GINTSTS.USBRST then
         GLOBAL_Periph.GINTSTS.USBRST := True;
         Put_Line ("GINTSTS.USBRST");

         DEVICE_Periph.DCTL.RWUSIG := False;

         Flush_TX_FIFO;

         --  FIXME: Not needed -> Flush_RX_FIFO;

         DEVICE_Periph.DIEPMSK.XFRCM := True;
         DEVICE_Periph.DIEPMSK.EPDM := True;
         DEVICE_Periph.DIEPMSK.TOM := True;

         DEVICE_Periph.DOEPMSK.XFRCM := True;
         DEVICE_Periph.DOEPMSK.EPDM := True;
         DEVICE_Periph.DOEPMSK.STUPM := True;
         DEVICE_Periph.DOEPMSK.OTEPDM := True; -- FIXME: Not needed?

         This.Set_Address (0);

         DEVICE_Periph.DOEPTSIZ0 :=
           (XFRSIZ  => 3 * 8,
            PKTCNT  => True,
            STUPCNT => 3,
            others => <>);

      end if;

      if GLOBAL_Periph.GINTSTS.ENUMDNE then

         GLOBAL_Periph.GINTSTS.ENUMDNE := True;
         --  TODO? fifo_mem_top
         Put_Line ("GINTSTS.ENUMDNE");

         return (Kind => Reset);
      end if;

      return No_Event;
   end Poll;

end DWC_OTG_FS;
