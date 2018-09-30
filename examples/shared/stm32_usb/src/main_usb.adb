------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with STM32_SVD.RCC; use STM32_SVD.RCC;
with HAL; use HAL;

with STM32.Device; use STM32.Device;
with STM32.GPIO; use STM32.GPIO;

with USB;
with USB.Device.HID;
with USB.Device.MIDI;

with DWC_OTG_FS;

with Interfaces; use Interfaces;

with Hex_Dump;

procedure Main_USB is

   Use_HID : constant Boolean := True;

   package DWC_OTG is new DWC_OTG_FS (16#50000000#);
   use DWC_OTG;

   UDC   : aliased OTG_USB_Device;
   Dev   : USB.USB_Device;
   Class_HID : aliased USB.Device.HID.Default_HID_Class;
   Class_MIDI : aliased USB.Device.MIDI.Default_MIDI_Class;

   Desc : aliased constant USB.Device_Descriptor :=
     (
      bLength            => USB.Device_Descriptor'Size / 8,
      bDescriptorType    => 1, -- DT_DEVICE
      bcdUSB             => 16#0200#,
      bDeviceClass       => 0,
      bDeviceSubClass    => 0,
      bDeviceProtocol    => 0,
      bMaxPacketSize0    => 64,
      idVendor           => 16#6666#,
      idProduct          => 16#4242#,
      bcdDevice          => 16#0100#,
      iManufacturer      => 1,
      iProduct           => 2,
      iSerialNumber      => 3,
      bNumConfigurations => 1
     );

   LANG_EN_US : constant USB.USB_String := (ASCII.HT, ASCII.EOT); -- 0x0409

   Strings : aliased constant USB.String_Array :=
     (
      (0, new USB.String_Descriptor'(2 + 2,  3, LANG_EN_US)),
      (1, new USB.String_Descriptor'(2 + 32, 3,
       ('S', ASCII.NUL,
        't', ASCII.NUL,
        'r', ASCII.NUL,
        ' ', ASCII.NUL,
        'M', ASCII.NUL,
        'a', ASCII.NUL,
        'n', ASCII.NUL,
        'u', ASCII.NUL,
        'f', ASCII.NUL,
        'a', ASCII.NUL,
        'c', ASCII.NUL,
        't', ASCII.NUL,
        'u', ASCII.NUL,
        'r', ASCII.NUL,
        'e', ASCII.NUL,
        'r', ASCII.NUL))),
      (2, new USB.String_Descriptor'(2 + 22, 3,
       ('S', ASCII.NUL,
        't', ASCII.NUL,
        'r', ASCII.NUL,
        ' ', ASCII.NUL,
        'P', ASCII.NUL,
        'r', ASCII.NUL,
        'o', ASCII.NUL,
        'd', ASCII.NUL,
        'u', ASCII.NUL,
        'c', ASCII.NUL,
        't', ASCII.NUL))),
      (3, new USB.String_Descriptor'(2 + 32, 3,
       ('S', ASCII.NUL,
        't', ASCII.NUL,
        'r', ASCII.NUL,
        ' ', ASCII.NUL,
        'S', ASCII.NUL,
        'e', ASCII.NUL,
        'r', ASCII.NUL,
        'i', ASCII.NUL,
        'a', ASCII.NUL,
        'l', ASCII.NUL,
        'N', ASCII.NUL,
        'u', ASCII.NUL,
        'm', ASCII.NUL,
        'b', ASCII.NUL,
        'e', ASCII.NUL,
        'r', ASCII.NUL)))
     );


   USB_DESC_TYPE_CONFIGURATION : constant := 2;
   USB_HID_CONFIG_DESC_SIZ     : constant := 34;
--     USB_DESC_TYPE_STRING        : constant := 3;
   USB_DESC_TYPE_INTERFACE     : constant := 4;
   USB_DESC_TYPE_ENDPOINT      : constant := 5;
   HID_DESCRIPTOR_TYPE         : constant := 16#21#;
--     HID_REPORT_DESC             : constant := 16#22#;
--     USB_HID_DESC_SIZ            : constant := 9;
   HID_MOUSE_REPORT_DESC_SIZE  : constant := 74;
   HID_EPIN_ADDR               : constant := 16#81#;
   HID_EPIN_SIZE               : constant := 16#04#;
   HID_FS_BINTERVAL            : constant := 16#0A#;

   Config_HID : aliased constant UInt8_Array :=
     (
      16#09#, --  bLength: Configuration Descriptor size */
      USB_DESC_TYPE_CONFIGURATION, --  bDescriptorType: Configuration */
      USB_HID_CONFIG_DESC_SIZ,
      --  wTotalLength: Bytes returned */
      16#00#,
      16#01#,         -- bNumInterfaces: 1 interface*/
      16#01#,         -- bConfigurationValue: Configuration value*/
      16#00#,         -- iConfiguration: Index of string descriptor describing
      --      the configuration*/
      16#E0#,         -- bmAttributes: bus powered and Support Remote Wake-up */
      16#32#,         -- MaxPower 100 mA: this current is used for detecting Vbus*/

      --  ************* Descriptor of Joystick Mouse interface ****************/
      --  09 */
      16#09#,         -- bLength: Interface Descriptor size*/
      USB_DESC_TYPE_INTERFACE, -- bDescriptorType: Interface descriptor type*/
      16#00#,         -- bInterfaceNumber: Number of Interface*/
      16#00#,         -- bAlternateSetting: Alternate setting*/
      16#01#,         -- bNumEndpoints*/
      16#03#,         -- bInterfaceClass: HID*/
      16#01#,         -- bInterfaceSubClass : 1=BOOT, 0=no boot*/
      16#02#,         -- nInterfaceProtocol : 0=none, 1=keyboard, 2=mouse*/
      0,            -- iInterface: Index of string descriptor*/
      --  ******************* Descriptor of Joystick Mouse HID ********************/
      --  18 */
      16#09#,         -- bLength: HID Descriptor size*/
      HID_DESCRIPTOR_TYPE, -- bDescriptorType: HID*/
      16#11#,         -- bcdHID: HID Class Spec release number*/
      16#01#,
      16#00#,         -- bCountryCode: Hardware target country*/
      16#01#,         -- bNumDescriptors: Number of HID class descriptors to follow*/
      16#22#,         -- bDescriptorType*/
      HID_MOUSE_REPORT_DESC_SIZE, -- wItemLength: Total length of Report descriptor*/
      16#00#,
      --  ******************* Descriptor of Mouse endpoint ********************/
      --  27 */
      16#07#,          -- bLength: Endpoint Descriptor size*/
      USB_DESC_TYPE_ENDPOINT, -- bDescriptorType:*/

      HID_EPIN_ADDR,     -- bEndpointAddress: Endpoint Address (IN)*/
      16#03#,          -- bmAttributes: Interrupt endpoint*/
      HID_EPIN_SIZE, -- wMaxPacketSize: 4 Byte max */
      16#00#,
      HID_FS_BINTERVAL          -- bInterval: Polling Interval (10 ms)*/
      --  34 */
     );

   USB_CFG_MAX_BUS_POWER : constant := 2;

   Config_MIDI : aliased constant UInt8_Array :=
     (
      --  USB configuration descriptor */
      9, --  sizeof(usbDescrConfig): length of descriptor in bytes */
      USB_DESC_TYPE_CONFIGURATION, --  descriptor type */
      101, 0, --  total length of data returned (including inlined descriptors) */
      2, --  number of interfaces in this configuration */
      1, --  index of this configuration */
      0, --  configuration name string index */
      Shift_Left (1, 7), --  attributes */
      USB_CFG_MAX_BUS_POWER / 2, --  max USB current in 2mA units */

      --  B.3 AudioControl Interface Descriptors
      --  The AudioControl interface describes the device structure (audio function topology)
      --  and is used to manipulate the Audio Controls. This device has no audio function
      --  incorporated. However, the AudioControl interface is mandatory and therefore both
      --  the standard AC interface descriptor and the classspecific AC interface descriptor
      --  must be present. The class-specific AC interface descriptor only contains the header
      --  descriptor.

      --  B.3.1 Standard AC Interface Descriptor
      --  The AudioControl interface has no dedicated endpoints associated with it. It uses the
      --  default pipe (endpoint 0) for all communication purposes. Class-specific AudioControl
      --  Requests are sent using the default pipe. There is no Status Interrupt endpoint provided.
      --  descriptor follows inline: */
      9, --  sizeof(usbDescrInterface): length of descriptor in bytes */
      USB_DESC_TYPE_INTERFACE, --  descriptor type */
      0, --  index of this interface */
      0, --  alternate setting for this interface */
      0, --  endpoints excl 0: number of endpoint descriptors to follow */
      1, --  */
      1, --  */
      0, --  */
      0, --  string index for interface */

      --  B.3.2 Class-specific AC Interface Descriptor
      --  The Class-specific AC interface descriptor is always headed by a Header descriptor
      --  that contains general information about the AudioControl interface. It contains all
      --  the pointers needed to describe the Audio Interface Collection, associated with the
      --  described audio function. Only the Header descriptor is present in this device
      --  because it does not contain any audio functionality as such.
      --  descriptor follows inline: */
      9, --  sizeof(usbDescrCDC_HeaderFn): length of descriptor in bytes */
      36, --  descriptor type */
      1, --  header functional descriptor */
      0, 0, --  bcdADC */
      9, 0, --  wTotalLength */
      1, --  */
      1, --  */

      --  B.4 MIDIStreaming Interface Descriptors

      --  B.4.1 Standard MS Interface Descriptor
      --  descriptor follows inline: */
      9, --  length of descriptor in bytes */
      USB_DESC_TYPE_INTERFACE, --  descriptor type */
      1, --  index of this interface */
      0, --  alternate setting for this interface */
      2, --  endpoints excl 0: number of endpoint descriptors to follow */
      1, --  AUDIO */
      3, --  MS */
      0, --  unused */
      0, --  string index for interface */

      --  B.4.2 Class-specific MS Interface Descriptor
      --  descriptor follows inline: */
      7, --  length of descriptor in bytes */
      36, --  descriptor type */
      1, --  header functional descriptor */
      0, 1, --  bcdADC */
      65, 0, --  wTotalLength */

      --  B.4.3 MIDI IN Jack Descriptor
      --  descriptor follows inline: */
      6, --  bLength */
      36, --  descriptor type */
      2, --  MIDI_IN_JACK desc subtype */
      1, --  EMBEDDED bJackType */
      1, --  bJackID */
      0, --  iJack */

      --  descriptor follows inline: */
      6, --  bLength */
      36, --  descriptor type */
      2, --  MIDI_IN_JACK desc subtype */
      2, --  EXTERNAL bJackType */
      2, --  bJackID */
      0, --  iJack */

      --  B.4.4 MIDI OUT Jack Descriptor
      --  descriptor follows inline: */
      9, --  length of descriptor in bytes */
      36, --  descriptor type */
      3, --  MIDI_OUT_JACK descriptor */
      1, --  EMBEDDED bJackType */
      3, --  bJackID */
      1, --  No of input pins */
      2, --  BaSourceID */
      1, --  BaSourcePin */
      0, --  iJack */

      --  descriptor follows inline: */
      9, --  bLength of descriptor in bytes */
      36, --  bDescriptorType */
      3, --  MIDI_OUT_JACK bDescriptorSubtype */
      2, --  EXTERNAL bJackType */
      4, --  bJackID */
      1, --  bNrInputPins */
      1, --  baSourceID (0) */
      1, --  baSourcePin (0) */
      0, --  iJack */

      --  B.5 Bulk OUT Endpoint Descriptors

      --  B.5.1 Standard Bulk OUT Endpoint Descriptor
      --  descriptor follows inline: */
      9, --  bLenght */
      USB_DESC_TYPE_ENDPOINT, --  bDescriptorType = endpoint */
      1, --  bEndpointAddress OUT endpoint number 1 */
      3, --  bmAttributes: 2:Bulk, 3:Interrupt endpoint */
      8, 0, --  wMaxPacketSize */
      10, --  bInterval in ms */
      0, --  bRefresh */
      0, --  bSyncAddress */

      --  B.5.2 Class-specific MS Bulk OUT Endpoint Descriptor
      --  descriptor follows inline: */
      5, --  bLength of descriptor in bytes */
      37, --  bDescriptorType */
      1, --  bDescriptorSubtype */
      1, --  bNumEmbMIDIJack  */
      1, --  baAssocJackID (0) */

      --  B.6 Bulk IN Endpoint Descriptors

      --  B.6.1 Standard Bulk IN Endpoint Descriptor
      --  descriptor follows inline: */
      9, --  bLenght */
      USB_DESC_TYPE_ENDPOINT, --  bDescriptorType = endpoint */
      16#81#, --  bEndpointAddress IN endpoint number 1 */
      3, --  bmAttributes: 2: Bulk, 3: Interrupt endpoint */
      8, 0, --  wMaxPacketSize */
      10, --  bInterval in ms */
      0, --  bRefresh */
      0, --  bSyncAddress */

      --  B.6.2 Class-specific MS Bulk IN Endpoint Descriptor
      --  descriptor follows inline: */
      5, --  bLength of descriptor in bytes */
      37, --  bDescriptorType */
      1, --  bDescriptorSubtype */
      1 --  bNumEmbMIDIJack (0) */
     );

   Next_TX : Time := Clock;
begin

   Enable_Clock (PA11);
   Enable_Clock (PA12);
   Enable_Clock (PA9);

   Configure_IO (PA9,
                 (Mode      => Mode_In,
                  Resistors => Floating));

   Configure_IO (PA11 & PA12,
                 (Mode     => Mode_AF,
                  Resistors => Floating,
                  AF_Output_Type => Push_Pull,
                  AF_Speed => Speed_Very_High,
                  AF => GPIO_AF_OTG_FS_10));


   RCC_Periph.AHB2ENR.OTGFSEN := True;

   if Use_HID then
      Dev.Initalize (UDC'Unchecked_Access,
                     Class_HID'Unchecked_Access,
                     Desc'Unchecked_Access,
                     Config_HID'Unchecked_Access,
                     Strings'Unchecked_Access);
   else
      Dev.Initalize (UDC'Unchecked_Access,
                     Class_MIDI'Unchecked_Access,
                     Desc'Unchecked_Access,
                     Config_MIDI'Unchecked_Access,
                     Strings'Unchecked_Access);
   end if;

   Dev.Start;

   loop
      Dev.Poll;

      if Use_HID then
         if Class_HID.Ready and then Clock > Next_TX then

            Class_HID.Set_Move (10, -10);

            Class_HID.Send_Report (UDC);

            Next_TX := Clock + Milliseconds (500);
         end if;
      else
         if Class_MIDI.Ready then
            Hex_Dump.Hex_Dump (Class_MIDI.Last, Ada.Text_IO.Put_Line'Access);
         end if;
      end if;
   end loop;

end Main_USB;

