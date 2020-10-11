--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;

with League.JSON.Objects;
with League.JSON.Values;

with Jupyter.Kernels;

procedure Magics.Ls_Magic
  (IO_Pub : not null Jupyter.Kernels.IO_Pub_Access;
   Silent : Boolean)
is
   Result : League.Strings.Universal_String;
   Data   : League.JSON.Objects.JSON_Object;
begin
   if not Silent then
      Result.Append ("Available line magics:");
      Result.Append (Ada.Characters.Wide_Wide_Latin_1.LF);
      Result.Append ("%lsmagic? %%output? %%writefile? ");
      Result.Append ("%gargs? %cargs? %largs? %bargs?");

      Data.Insert (+"text/plain", League.JSON.Values.To_JSON_Value (Result));
      IO_Pub.Execute_Result
        (Data      => Data,
         Metadata  => League.JSON.Objects.Empty_JSON_Object,
         Transient => League.JSON.Objects.Empty_JSON_Object);
   end if;
end Magics.Ls_Magic;
