--  SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

package Embedded.Texts is
   Text : Embedded_Text := (
     +"ada_driver.adb",
     +"--  SPDX-FileCopyrightText: 2020-2022 Max Reznik <reznikmm@gmail.com>",
     +"--",
     +"--  SPDX-License-Identifier: MIT",
     +"---------------------------------------------------------------------",
     +"",
     +"with Ada.Characters.Latin_1;",
     +"with Ada.Text_IO;",
     +"",
     +"with Load_Library;",
     +"procedure Ada_Driver is",
     +"   procedure Service_Message (Text : String);",
     +"   --  Inject a service message into stderr stream",
     +"",
     +"   ---------------------",
     +"   -- Service_Message --",
     +"   ---------------------",
     +"",
     +"   procedure Service_Message (Text : String) is",
     +"      Switch : constant String := (1 => Ada.Characters.Latin_1.NUL);",
     +"   begin",
     +"      Ada.Text_IO.Put",
     +"        (Ada.Text_IO.Standard_Error, Switch & Text & Switch);",
     +"   end Service_Message;",
     +"",
     +"begin",
     +"   while not Ada.Text_IO.End_Of_File loop",
     +"      declare",
     +"         Line : constant String := Ada.Text_IO.Get_Line;",
     +"      begin",
     +"         if Line'Length > 5 and Line (1 .. 5) = ~Load ~ then",
     +"            declare",
     +"               Error : constant String :=",
     +"                 Load_Library (Line (6 .. Line'Last));",
     +"            begin",
     +"               if Error /= ~~ then",
     +"                  Service_Message (~%jad_error:~ & Error);",
     +"               end if;",
     +"            end;",
     +"         elsif Line'Length > 5 and Line (1 .. 4) = ~Exit~ then",
     +"            exit;",
     +"         else",
     +"            Service_Message (~%jad_bad:~ & Line);",
     +"         end if;",
     +"      end;",
     +"      Service_Message (~%jad_ready~);",
     +"   end loop;",
     +"end Ada_Driver;",
     +"END ada_driver.adb",
     +"load_library.adb",
     +"--  SPDX-FileCopyrightText: 2020-2022 Max Reznik <reznikmm@gmail.com>",
     +"--",
     +"--  SPDX-License-Identifier: MIT",
     +"---------------------------------------------------------------------",
     +"",
     +"with Interfaces.C.Strings;",
     +"with System;",
     +"",
     +"function Load_Library",
     +"  (Name  : Ada.Strings.UTF_Encoding.UTF_8_String)",
     +"    return Ada.Strings.UTF_Encoding.UTF_8_String",
     +"is",
     +"   use type System.Address;",
     +"",
     +"   RTLD_NOW    : constant Interfaces.C.int := 2;",
     +"",
     +"   function dlopen",
     +"     (file : Interfaces.C.char_array;",
     +"      mode : Interfaces.C.int) return System.Address",
     +"     with Import, Convention => C, External_Name => ~dlopen~;",
     +"",
     +"   function dlerror return Interfaces.C.Strings.chars_ptr",
     +"     with Import, Convention => C, External_name => ~dlerror~;",
     +"",
     +"   Raw_Name : constant Interfaces.C.char_array :=",
     +"     Interfaces.C.To_C (Name);",
     +"",
     +"   Handler  : constant System.Address := dlopen (Raw_Name, RTLD_NOW);",
     +"   Message  : constant Interfaces.C.Strings.chars_ptr := dlerror;",
     +"begin",
     +"   if Handler = System.Null_Address then",
     +"      return Interfaces.C.Strings.Value (Message);",
     +"   else",
     +"      return ~~;",
     +"   end if;",
     +"end Load_Library;",
     +"END load_library.adb",
     +"load_library.ads",
     +"--  SPDX-FileCopyrightText: 2020-2022 Max Reznik <reznikmm@gmail.com>",
     +"--",
     +"--  SPDX-License-Identifier: MIT",
     +"---------------------------------------------------------------------",
     +"",
     +"with Ada.Strings.UTF_Encoding;",
     +"",
     +"function Load_Library",
     +"  (Name  : Ada.Strings.UTF_Encoding.UTF_8_String)",
     +"    return Ada.Strings.UTF_Encoding.UTF_8_String;",
     +"--  Load a shared library with given Name and return error or an empty",
     +"--  string if success.",
     +"END load_library.ads",
     +"");
end Embedded.Texts;
