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

generic
   type Object is private;
   type Name is access all Object;
package Flexible_Memory_Pools is

   type Pool (Pre_Allocated : Natural;
              Use_Heap      : Boolean) is private;

   function Allocate (This : in out Pool) return Name;
   procedure Deallocate (This : in out Pool; Ptr : in out Name);
private

   type Object_Array is array (Natural range <>) of aliased Object;
   type Free_Flag_Array is array (Natural range <>) of aliased Boolean;

   ------------------------
   -- Pre_Allocated_Pool --
   ------------------------

   type Pre_Allocated_Pool (Size : Natural) is record
      Static     : Object_Array (1 .. Size);
      Free       : Free_Flag_Array (1 .. Size) := (others => True);
      First_Free : Natural := 1;
      Used_Cnt   : Natural := 0;
   end record;

   function Full (This : Pre_Allocated_Pool) return Boolean;

   function Is_In (This : Pre_Allocated_Pool; Ptr : Name) return Boolean;

   function Allocate (This : in out Pre_Allocated_Pool) return Name
     with Pre  => not Full (This),
          Post => Is_In (This, Allocate'Result);

   procedure Deallocate (This : in out Pre_Allocated_Pool; Ptr : in out Name)
     with Pre  => Is_In (This, Ptr),
          Post => not Full (This);


   ---------------------
   -- Heap_Allocation --
   ---------------------

   type Holder;
   type Holder_Ref is access all Holder;

   type Holder is record
      Obj  : aliased Object;
      Free : Boolean := True;
      Next : Holder_Ref := null;
   end record;

   type Heap_Allocation is record
      List     : Holder_Ref := null;
      Free_Cnt : Natural := 0;
   end record;

   function Allocate (This : in out Heap_Allocation) return Name;

   procedure Deallocate (This : in out Heap_Allocation; Ptr : in out Name);

   ----------
   -- Pool --
   ----------

   type Pool (Pre_Allocated : Natural;
              Use_Heap      : Boolean) is
   record
         Static : Pre_Allocated_Pool (Pre_Allocated);
         Heap   : Heap_Allocation;
   end record;

end Flexible_Memory_Pools;
