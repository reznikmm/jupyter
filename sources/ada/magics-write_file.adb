--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with Jupyter.Kernels;

procedure Magics.Write_File
  (IO_Pub : not null Jupyter.Kernels.IO_Pub_Access;
   Name   : League.Strings.Universal_String;
   Text   : League.Strings.Universal_String;
   Silent : Boolean)
is
   pragma Unreferenced (Silent, IO_Pub);
   Output : Ada.Wide_Wide_Text_IO.File_Type;
begin
   Ada.Wide_Wide_Text_IO.Create
     (Output, Name => Name.To_UTF_8_String, Form => "WCEM=8");
   Ada.Wide_Wide_Text_IO.Put_Line (Output, Text.To_Wide_Wide_String);
   Ada.Wide_Wide_Text_IO.Close (Output);
end Magics.Write_File;
