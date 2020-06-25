--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;

with League.JSON.Values;
with League.String_Vectors;

with Magics.Ls_Magic;

package body Ada_Kernels is
   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function "-" (Text : Wide_Wide_String)
     return League.JSON.Values.JSON_Value is
       (League.JSON.Values.To_JSON_Value (+Text));

   procedure Parse_Line_Magic
     (Code : League.Strings.Universal_String;
      Args : out League.String_Vectors.Universal_String_Vector);

   procedure Execute_Magic
     (Self              : aliased in out Session'Class;
      IO_Pub            : not null Jupyter.Kernels.IO_Pub_Access;
      Execution_Counter : Positive;
      Magic             : League.String_Vectors.Universal_String_Vector;
      Silent            : Boolean;
      Error             : in out Jupyter.Kernels.Execution_Error);

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
      pragma Unreferenced (User_Expressions, Allow_Stdin,
                           Stop_On_Error, Expression_Values);
      Data : League.JSON.Objects.JSON_Object;
      Meta : League.JSON.Objects.JSON_Object;
      Text : constant League.Strings.Universal_String := Code;
   begin
      declare
         Magic : League.String_Vectors.Universal_String_Vector;
      begin
         Parse_Line_Magic (Code, Magic);

         if Magic.Length >= 1 then
            Self.Execute_Magic
              (IO_Pub, Execution_Counter, Magic, Silent, Error);
            return;
         end if;
      end;

      Expression_Values := League.JSON.Objects.Empty_JSON_Object;

      if not Silent then
         Data.Insert (+"text/plain", League.JSON.Values.To_JSON_Value (Text));
         IO_Pub.Execute_Result
           (Data      => Data,
            Metadata  => Meta,
            Transient => League.JSON.Objects.Empty_JSON_Object);
      end if;
   end Execute;

   -------------------
   -- Execute_Magic --
   -------------------

   procedure Execute_Magic
     (Self              : aliased in out Session'Class;
      IO_Pub            : not null Jupyter.Kernels.IO_Pub_Access;
      Execution_Counter : Positive;
      Magic             : League.String_Vectors.Universal_String_Vector;
      Silent            : Boolean;
      Error             : in out Jupyter.Kernels.Execution_Error)
   is
      pragma Unreferenced (Self, Execution_Counter);
      use type League.Strings.Universal_String;

      First : constant League.Strings.Universal_String := Magic (1);
   begin
      if First = +"%lsmagic" then
         Magics.Ls_Magic (IO_Pub, Silent);
      else
         Error.Name := +"UsageError";
         Error.Value := "Line magic function `" & First & "` not found.";

         IO_Pub.Stream
           (Name => +"stderr",
            Text => Error.Name & ": " & Error.Value);
      end if;
   end Execute_Magic;

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

   ----------------------
   -- Parse_Line_Magic --
   ----------------------

   procedure Parse_Line_Magic
     (Code : League.Strings.Universal_String;
      Args : out League.String_Vectors.Universal_String_Vector)
   is
      Lines : constant League.String_Vectors.Universal_String_Vector :=
        Code.Split
          (Ada.Characters.Wide_Wide_Latin_1.LF,
           League.Strings.Skip_Empty);
   begin
      for J in 1 .. Lines.Length loop
         declare
            Line : constant League.Strings.Universal_String := Lines (J);
            Words : constant League.String_Vectors.Universal_String_Vector :=
              Line.Split (' ', League.Strings.Skip_Empty);
         begin
            if Line.Starts_With ("%") and not Line.Starts_With ("%%") then
               Args := Words;
               exit;
            end if;
         end;
      end loop;
   end Parse_Line_Magic;

end Ada_Kernels;
