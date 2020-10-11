--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.String_Vectors;
with League.JSON.Objects;
with League.JSON.Values;

with Jupyter.Kernels;

procedure Magics.Gprbuild_Options
  (IO_Pub  : not null Jupyter.Kernels.IO_Pub_Access;
   Silent  : Boolean;
   Options : in out League.String_Vectors.Universal_String_Vector;
   Section : League.Strings.Universal_String;
   Args    : League.String_Vectors.Universal_String_Vector)
is
   Result : League.Strings.Universal_String;
   Data   : League.JSON.Objects.JSON_Object;
begin
   if Args.Is_Empty then
      Options.Clear;
   else
      Options.Append (Args);
   end if;

   if not Silent then
      if Options.Is_Empty then
         Result.Append ("Now there is no ");
         Result.Append (Section);
         Result.Append ("options");
      else
         Result.Append ("Now ");
         Result.Append (Section);
         Result.Append (" options are: ");
         Result.Append (Options.Join (" "));
      end if;

      Data.Insert (+"text/plain", League.JSON.Values.To_JSON_Value (Result));
      IO_Pub.Execute_Result
        (Data      => Data,
         Metadata  => League.JSON.Objects.Empty_JSON_Object,
         Transient => League.JSON.Objects.Empty_JSON_Object);
   end if;
end Magics.Gprbuild_Options;
