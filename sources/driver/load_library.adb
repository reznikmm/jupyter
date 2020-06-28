--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Interfaces.C.Strings;
with System;

procedure Load_Library
  (Name  : League.Strings.Universal_String;
   Error : out League.Strings.Universal_String)
is
   use type System.Address;

   RTLD_NOW    : constant Interfaces.C.int := 2;

   function dlopen
     (file : Interfaces.C.char_array;
      mode : Interfaces.C.int) return System.Address
     with Import, Convention => C, External_Name => "dlopen";

   function dlerror return Interfaces.C.Strings.chars_ptr
     with Import, Convention => C, External_name => "dlerror";

   Raw_Name : constant Interfaces.C.char_array :=
     Interfaces.C.To_C (Name.To_UTF_8_String);

   Handler  : constant System.Address := dlopen (Raw_Name, RTLD_NOW);
   Message  : constant Interfaces.C.Strings.chars_ptr := dlerror;
begin
   if Handler = System.Null_Address then
      Error := League.Strings.From_UTF_8_String
        (Interfaces.C.Strings.Value (Message));
   end if;
end Load_Library;
