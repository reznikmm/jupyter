--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

separate (Jupyter.Start_Kernel)
package body IO_Pubs is

   ------------
   -- Stream --
   ------------

   overriding procedure Stream
     (Self : in out IO_Pub;
      Name : League.Strings.Universal_String;
      Text : League.Strings.Universal_String)
   is
      Content : League.JSON.Objects.JSON_Object;
   begin
      Content.Insert
        (+"name",
         League.JSON.Values.To_JSON_Value (Name));
      Content.Insert
        (+"text",
         League.JSON.Values.To_JSON_Value (Text));

      Send_Message
        (Self.Up.IOPub,
         -(+"stream"),
         Self.Up.Key,
         "stream",
         Self.Request,
         Content);
   end Stream;

   overriding procedure Display_Data
     (Self      : in out IO_Pub;
      Data      : League.JSON.Objects.JSON_Object;
      Metadata  : League.JSON.Objects.JSON_Object;
      Transient : League.JSON.Objects.JSON_Object) is null;

   overriding procedure Update_Display_Data
     (Self      : in out IO_Pub;
      Data      : League.JSON.Objects.JSON_Object;
      Metadata  : League.JSON.Objects.JSON_Object;
      Transient : League.JSON.Objects.JSON_Object) is null;

   --------------------
   -- Execute_Result --
   --------------------

   overriding procedure Execute_Result
     (Self      : in out IO_Pub;
      Data      : League.JSON.Objects.JSON_Object;
      Metadata  : League.JSON.Objects.JSON_Object;
      Transient : League.JSON.Objects.JSON_Object)
   is
      Content : League.JSON.Objects.JSON_Object;
   begin
      Content.Insert
        (+"execution_count",
         League.JSON.Values.To_JSON_Value
           (League.Holders.Universal_Integer (Self.Count)));
      Content.Insert (+"data", Data.To_JSON_Value);
      Content.Insert (+"metadata", Metadata.To_JSON_Value);
      Content.Insert (+"transient", Transient.To_JSON_Value);

      Send_Message
        (Self.Up.IOPub,
         -(+"execute_result"),
         Self.Up.Key,
         "execute_result",
         Self.Request,
         Content);
   end Execute_Result;

   overriding procedure Execute_Error
     (Self  : in out IO_Pub;
      Value : Jupyter.Kernels.Execution_Error) is null;

   overriding procedure Clear_Output
     (Self  : in out IO_Pub;
      Wait  : Boolean) is null;

   overriding procedure Debug_Event
     (Self      : in out IO_Pub;
      Content   : League.JSON.Objects.JSON_Object) is null;

end IO_Pubs;
