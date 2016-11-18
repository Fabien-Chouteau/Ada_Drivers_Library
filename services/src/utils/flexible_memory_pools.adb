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

with System.Storage_Elements; use System.Storage_Elements;

package body Flexible_Memory_Pools is

   function Allocate (This : in out Pool) return Name is
   begin

      if not Full (This.Static) then
         return Allocate (This.Static);
      elsif This.Use_Heap then
         return Allocate (This.Heap);
      else
         return null;
      end if;
   end Allocate;


   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (This : in out Pool; Ptr : in out Name) is
   begin
      if Is_In (This.Static, Ptr) then
         Deallocate (This.Static, Ptr);
      elsif This.Use_Heap then
         Deallocate (This.Heap, Ptr);
      else
         raise Program_Error with "Ptr doesn't belong to this pool";
      end if;
   end Deallocate;

   ----------
   -- Full --
   ----------

   function Full (This : Pre_Allocated_Pool) return Boolean is
      (This.Used_Cnt = This.Size);

   -----------
   -- Is_In --
   -----------

   function Is_In (This : Pre_Allocated_Pool; Ptr : Name) return Boolean is
   begin
      if This.Size = 0 then
         return False;
      end if;

      declare
         First : constant Integer_Address :=
           To_Integer (This.Static (This.Static'First)'Address);
         Last : constant Integer_Address :=
           To_Integer (This.Static (This.Static'Last)'Address);
         Ptr_Addr : constant Integer_Address :=
           To_Integer (Ptr.all'Address);
      begin
         return This.Size /= 0 and then Ptr_Addr in First .. Last;
      end;
   end Is_In;

   --------------
   -- Allocate --
   --------------

   function Allocate (This : in out Pre_Allocated_Pool) return Name is
      Ret : Name;
   begin
      pragma Assert (This.Free (This.First_Free));

      Ret := This.Static (This.First_Free)'Unchecked_Access;
      This.Free (This.First_Free) := False;
      This.Used_Cnt := This.Used_Cnt + 1;

      if not Full (This) then
         for Index in This.Static'Range loop
            if This.Free (Index) then
               This.First_Free := Index;
               exit;
            end if;
         end loop;
      end if;

      return Ret;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (This : in out Pre_Allocated_Pool; Ptr : in out Name) is
      Tmp : Name;
   begin
      for Index in This.Static'Range loop
         Tmp := This.Static (Index)'Unchecked_Access;
         if Ptr = Tmp then
            This.First_Free := Index;
            This.Free (Index) := True;
            This.Used_Cnt := This.Used_Cnt - 1;
            Ptr := null;
            return;
         end if;
      end loop;
      raise Program_Error with "Ptr not found in the pool";
   end Deallocate;

   --------------
   -- Allocate --
   --------------

   function Allocate (This : in out Heap_Allocation) return Name is
      Hld : Holder_Ref;
   begin
      if This.Free_Cnt /= 0 then
         Hld := This.List;
         while Hld /= null loop
            if Hld.Free then
               Hld.Free := False;
               return Hld.Obj'Unchecked_Access;
            else
               Hld := Hld.Next;
            end if;
         end loop;
         raise Program_Error with "There should be a free object in the list";
      else
         Hld := new Holder;
         Hld.Free := False;
         Hld.Next := This.List;
         This.List := Hld;
         return Hld.Obj'Unchecked_Access;
      end if;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (This : in out Heap_Allocation; Ptr : in out Name) is
      Hld : Holder_Ref := This.List;
   begin
      while Hld /= null loop
         if Hld.Obj'Unchecked_Access = Ptr then
            Hld.Free := True;
            This.Free_Cnt := This.Free_Cnt + 1;
            Ptr := null;
            return;
         else
            Hld := Hld.Next;
         end if;
      end loop;
      raise Program_Error with "Ptr should be in the list of object";
   end Deallocate;

end Flexible_Memory_Pools;
