--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Wide_Wide_Text_IO;

with League.Strings;
with Load_Library;
procedure Ada_Driver is
   procedure Service_Message (Text : Wide_Wide_String);
   --  Inject a service message into stderr stream

   ---------------------
   -- Service_Message --
   ---------------------

   procedure Service_Message (Text : Wide_Wide_String) is
      Switch : constant Wide_Wide_String :=
        (1 => Ada.Characters.Wide_Wide_Latin_1.NUL);
   begin
      Ada.Wide_Wide_Text_IO.Put
        (Ada.Wide_Wide_Text_IO.Standard_Error,
         Switch & Text & Switch);
   end Service_Message;

begin
   while not Ada.Wide_Wide_Text_IO.End_Of_File loop
      declare
         Error : League.Strings.Universal_String;
         Line  : constant League.Strings.Universal_String :=
           League.Strings.To_Universal_String
             (Ada.Wide_Wide_Text_IO.Get_Line);
      begin
         if Line.Starts_With ("Load ") then
            Load_Library (Line.Tail_From (6), Error);

            if not Error.Is_Empty then
               Service_Message ("%jad_error:" & Error.To_Wide_Wide_String);
            end if;
         elsif Line.Starts_With ("Exit") then
            exit;
         else
            Service_Message ("%jad_bad:" & Line.To_Wide_Wide_String);
         end if;
      end;
      Service_Message ("%jad_ready");
   end loop;
end Ada_Driver;
