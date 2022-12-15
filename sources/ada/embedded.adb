--  SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;
with League.String_Vectors;
with Embedded.Texts;

package body Embedded is

   -----------------
   -- Write_Texts --
   -----------------

   procedure Write_Texts (Directory : League.Strings.Universal_String) is
      use type League.Strings.Universal_String;

      Output : Ada.Wide_Wide_Text_IO.File_Type;
      Dir    : constant String := Directory.To_UTF_8_String;
      Marker : League.Strings.Universal_String;
   begin
      for Line of Embedded.Texts.Text loop
         if Line = Marker then
            Ada.Wide_Wide_Text_IO.Close (Output);
         elsif Ada.Wide_Wide_Text_IO.Is_Open (Output) then
            Ada.Wide_Wide_Text_IO.Put_Line
              (Output, Line.Split ('~').Join ("""").To_Wide_Wide_String);
         elsif not Line.Is_Empty then
            Marker.Clear;
            Marker.Append ("END ");
            Marker.Append (Line);
            Ada.Wide_Wide_Text_IO.Create
              (Output,
               Ada.Wide_Wide_Text_IO.Out_File,
               Dir & Line.To_UTF_8_String,
               "shared=no");
         end if;
      end loop;
   end Write_Texts;

end Embedded;
