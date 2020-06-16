--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.JSON.Values;

package body Hello_World is
   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function "-" (Text : Wide_Wide_String)
     return League.JSON.Values.JSON_Value is
       (League.JSON.Values.To_JSON_Value (+Text));

   --------------------
   -- Create_Session --
   --------------------

   overriding procedure Create_Session
     (Self       : aliased in out Kernel;
      Session_Id : Positive;
      Result     : out Jupyter.Kernels.Session_Access)
   is
      Object : constant Session_Access := new Session;
   begin
      Result := Jupyter.Kernels.Session_Access (Object);
      Self.Map.Insert (Session_Id, Object);
   end Create_Session;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self              : aliased in out Session;
      IO_Pub            : not null Jupyter.Kernels.IO_Pub_Access;
      Execution_Counter : Positive;
      Code              : League.Strings.Universal_String;
      Silent            : Boolean;
      User_Expressions  : League.JSON.Objects.JSON_Object;
      Allow_Stdin       : Boolean;
      Stop_On_Error     : Boolean;
      Expression_Values : out League.JSON.Objects.JSON_Object;
      Error             : in out Jupyter.Kernels.Execution_Error)
   is
      pragma Unreferenced (Execution_Counter, User_Expressions, Allow_Stdin,
                           Stop_On_Error, Expression_Values, Error, Self);
      Data : League.JSON.Objects.JSON_Object;
      Meta : League.JSON.Objects.JSON_Object;
   begin
      Expression_Values := League.JSON.Objects.Empty_JSON_Object;

      if not Silent then
         Data.Insert (+"text/plain", League.JSON.Values.To_JSON_Value (Code));
         IO_Pub.Execute_Result
           (Data      => Data,
            Metadata  => Meta,
            Transient => League.JSON.Objects.Empty_JSON_Object);
      end if;
   end Execute;

   -----------------
   -- Get_Session --
   -----------------

   overriding function Get_Session
     (Self       : aliased in out Kernel;
      Session_Id : Positive) return Jupyter.Kernels.Session_Access
   is
      Result : constant Session_Access := Self.Map (Session_Id);
   begin
      return Jupyter.Kernels.Session_Access (Result);
   end Get_Session;

   -----------------
   -- Kernel_Info --
   -----------------

   overriding procedure Kernel_Info
     (Self   : aliased in out Kernel;
      Result : out League.JSON.Objects.JSON_Object)
   is
      pragma Unreferenced (Self);
      Language : League.JSON.Objects.JSON_Object;
   begin
      Language.Insert (+"name", -"Hello World");
      Language.Insert (+"version", -"2012");
      Language.Insert (+"mimetype", -"text/x-ada");
      Language.Insert (+"file_extension", -".adb");
      Language.Insert (+"pygments_lexer", -"ada");
      Language.Insert (+"codemirror_mode", -"ada");
      Result.Insert (+"protocol_version", -"5.3");
      Result.Insert (+"implementation", -"dummy");
      Result.Insert (+"implementation_version", -"0.1.0");
      Result.Insert (+"language_info", Language.To_JSON_Value);
      Result.Insert (+"banner", -"Some banner.");
      Result.Insert (+"status", -"ok");
   end Kernel_Info;

end Hello_World;
