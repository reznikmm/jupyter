--  SPDX-FileCopyrightText: 2020-2022 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
---------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Text_IO;

with Load_Library;
procedure Ada_Driver is
   procedure Service_Message (Text : String);
   --  Inject a service message into stderr stream

   ---------------------
   -- Service_Message --
   ---------------------

   procedure Service_Message (Text : String) is
      Switch : constant String := (1 => Ada.Characters.Latin_1.NUL);
   begin
      Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Switch & Text & Switch);
   end Service_Message;

begin
   while not Ada.Text_IO.End_Of_File loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         if Line'Length > 5 and Line (1 .. 5) = "Load " then
            declare
               Error : constant String := Load_Library (Line (6 .. Line'Last));
            begin
               if Error /= "" then
                  Service_Message ("%jad_error:" & Error);
               end if;
            end;
         elsif Line'Length > 5 and Line (1 .. 4) = "Exit" then
            exit;
         else
            Service_Message ("%jad_bad:" & Line);
         end if;
      end;
      Service_Message ("%jad_ready");
   end loop;
end Ada_Driver;
