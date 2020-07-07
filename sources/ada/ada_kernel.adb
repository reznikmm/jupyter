--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Directories;
with Ada.Environment_Variables;
with League.Application;
with League.String_Vectors;
with League.Strings;

with Ada_Kernels;
with Jupyter.Start_Kernel;

procedure Ada_Kernel is
   function Get_Top_Directory return League.Strings.Universal_String;

   -----------------------
   -- Get_Top_Directory --
   -----------------------

   function Get_Top_Directory return League.Strings.Universal_String is

      function Append (Root : String) return League.Strings.Universal_String;

      ------------
      -- Append --
      ------------

      function Append (Root : String) return League.Strings.Universal_String is
         Value  : constant String := Ada.Directories.Compose (Root, "jupyter");
         Result : League.Strings.Universal_String;
      begin
         Ada.Directories.Create_Path (Value);
         Result := League.Strings.From_UTF_8_String (Value);
         Result.Append ('/');

         return Result;
      end Append;

      Tmp  : constant String := Ada.Environment_Variables.Value ("TMPDIR", "");
      Temp : constant String := Ada.Environment_Variables.Value ("TEMP", "");
   begin
      if Tmp /= "" then
         return Append (Tmp);
      elsif Temp /= "" then
         return Append (Temp);
      else
         return Append ("/tmp");
      end if;
   end Get_Top_Directory;

   Kernel : Ada_Kernels.Kernel;
   Error  : League.Strings.Universal_String;
   Args   : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;
begin
   Kernel.Initialize (Get_Top_Directory, Error);

   if Error.Is_Empty then
      Jupyter.Start_Kernel (Kernel, Args (1));
   else
      raise Program_Error with Error.To_UTF_8_String;
   end if;
end Ada_Kernel;
