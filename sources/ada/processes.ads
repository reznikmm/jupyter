--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Strings;
with League.String_Vectors;

package Processes is

   type Environment is record
      Names  : League.String_Vectors.Universal_String_Vector;
      Values : League.String_Vectors.Universal_String_Vector;
   end record;

   No_Env : constant Environment := (others => <>);

   procedure Run
     (Program   : League.Strings.Universal_String;
      Arguments : League.String_Vectors.Universal_String_Vector;
      Directory : League.Strings.Universal_String;
      Env       : Environment := No_Env;
      Output    : out League.Strings.Universal_String;
      Errors    : out League.Strings.Universal_String;
      Status    : out Integer);

end Processes;
