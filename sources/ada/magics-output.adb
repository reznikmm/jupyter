--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.JSON.Documents;
with League.JSON.Objects;
with League.JSON.Values;

with Jupyter.Kernels;

procedure Magics.Output
  (IO_Pub : not null Jupyter.Kernels.IO_Pub_Access;
   MIME   : League.Strings.Universal_String;
   Text   : League.Strings.Universal_String;
   Silent : Boolean)
is
   use type League.Strings.Universal_String;
   Data   : League.JSON.Objects.JSON_Object;
   Meta   : League.JSON.Objects.JSON_Object;
begin
   if not Silent then

      if MIME = +"application/json" then
         declare
            Doc : constant League.JSON.Documents.JSON_Document :=
              League.JSON.Documents.From_JSON (Text);
            Expanded : League.JSON.Objects.JSON_Object;
         begin
            Expanded.Insert
              (+"expanded", League.JSON.Values.To_JSON_Value (True));

            Meta.Insert (MIME, Expanded.To_JSON_Value);

            if Doc.Is_Array then
               Data.Insert (MIME, Doc.To_JSON_Array.To_JSON_Value);
            else
               Data.Insert (MIME, Doc.To_JSON_Object.To_JSON_Value);
            end if;
         end;
      else
         Data.Insert (MIME, League.JSON.Values.To_JSON_Value (Text));
      end if;

      IO_Pub.Execute_Result
        (Data      => Data,
         Metadata  => Meta,
         Transient => League.JSON.Objects.Empty_JSON_Object);
   end if;
end Magics.Output;
