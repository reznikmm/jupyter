--  SPDX-FileCopyrightText: 2020-2022 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
---------------------------------------------------------------------

with Interfaces.C.Strings;
with System;

function Load_Library
  (Name  : Ada.Strings.UTF_Encoding.UTF_8_String)
    return Ada.Strings.UTF_Encoding.UTF_8_String
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
     Interfaces.C.To_C (Name);

   Handler  : constant System.Address := dlopen (Raw_Name, RTLD_NOW);
   Message  : constant Interfaces.C.Strings.chars_ptr := dlerror;
begin
   if Handler = System.Null_Address then
      return Interfaces.C.Strings.Value (Message);
   else
      return "";
   end if;
end Load_Library;
