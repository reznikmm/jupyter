--  SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Strings;

package Embedded is
   type Embedded_Text is array (Positive range <>) of
     League.Strings.Universal_String;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   procedure Write_Texts (Directory : League.Strings.Universal_String);

end Embedded;
