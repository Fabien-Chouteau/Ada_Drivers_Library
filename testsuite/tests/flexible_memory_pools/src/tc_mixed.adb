------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
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

with Flexible_Memory_Pools;
with Generic_Allocation_Test;
with Ada.Text_IO;

procedure TC_Mixed is
   type Some_Data is record
      A : Integer;
      B : Float;
      C : Natural;
   end record;
   type Some_Data_Ref is access all Some_Data;

   package Data_Pools is new Flexible_Memory_Pools (Some_Data, Some_Data_Ref);
   use Data_Pools;

   package Alloc_Test is new Generic_Allocation_Test
     (Number_Of_Allocation => 6,
      Pools                => Data_Pools);

   Data_Pool : Data_Pools.Pool (Pre_Allocated => 3,
                                Use_Heap      => True);
begin

   Alloc_Test.Test_Allocation (Data_Pool);

   if Allocate (Data_Pool) = null then
      raise Program_Error with "Allocation should still be possible";
   end if;

   Alloc_Test.Test_Deallocation (Data_Pool);

   Ada.Text_IO.Put_Line ("Test OK");
end TC_Mixed;
