--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Directories;
with Ada.Streams;
with Ada.Wide_Wide_Text_IO;

with GNAT.OS_Lib;

with League.JSON.Values;
with League.String_Vectors;
with League.Text_Codecs;

with Magics.Ls_Magic;
with Processes;

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
   --  Extract one line magic from Code, split it and put it into Args.

   function Find_In_Path
     (File : String) return League.Strings.Universal_String;

   procedure Execute_Magic
     (Self              : aliased in out Session'Class;
      IO_Pub            : not null Jupyter.Kernels.IO_Pub_Access;
      Execution_Counter : Positive;
      Magic             : League.String_Vectors.Universal_String_Vector;
      Silent            : Boolean;
      Error             : in out Jupyter.Kernels.Execution_Error);
   --  Execute one line magic command.

   function With_Clause_Probe
     (Self : aliased in out Session'Class;
      Code : League.Strings.Universal_String) return Boolean;
   --  Check if given Code is a `clause` before a compilation unit

   type Compilation_Unit is record
      Name : League.Strings.Universal_String;
      Text : League.Strings.Universal_String;
   end record;

   function Compilation_Unit_Probe
     (Self : aliased in out Session'Class;
      Unit : Compilation_Unit) return Boolean;
   --  Check if given Unit is a valid compilation unit

   Top_Dir : constant League.Strings.Universal_String := +"/tmp/jupyter/";

   ----------------------------
   -- Compilation_Unit_Probe --
   ----------------------------

   function Compilation_Unit_Probe
     (Self : aliased in out Session'Class;
      Unit : Compilation_Unit) return Boolean
   is
      use type League.Strings.Universal_String;

      Output  : Ada.Wide_Wide_Text_IO.File_Type;
      Args    : League.String_Vectors.Universal_String_Vector;
      Listing : League.Strings.Universal_String;
      Errors  : League.Strings.Universal_String;
      Status  : Integer;
   begin
      Ada.Wide_Wide_Text_IO.Create
        (Output,
         Name => League.Strings.To_UTF_8_String (Self.Directory & Unit.Name),
         Form => "WCEM=8");

      Ada.Wide_Wide_Text_IO.Put_Line (Output, Unit.Text.To_Wide_Wide_String);

      Ada.Wide_Wide_Text_IO.Close (Output);

      Ada.Wide_Wide_Text_IO.Create
        (Output,
         Name => Self.Directory.To_UTF_8_String & "try.gpr",
         Form => "WCEM=8");

      Ada.Wide_Wide_Text_IO.Put_Line (Output, "project Try is end Try;");

      Ada.Wide_Wide_Text_IO.Close (Output);

      Args.Append (+"-Ptry.gpr");
      Args.Append (+"-c");
      Args.Append (Unit.Name);

      Processes.Run
        (Program   => Self.Gprbuild,
         Arguments => Args,
         Directory => Self.Directory,
         Output    => Listing,
         Errors    => Errors,
         Status    => Status);

      return Status = 0;
   end Compilation_Unit_Probe;

   --------------------
   -- Create_Session --
   --------------------

   overriding procedure Create_Session
     (Self       : aliased in out Kernel;
      Session_Id : Positive;
      Result     : out Jupyter.Kernels.Session_Access)
   is
      Dir    : League.Strings.Universal_String := Top_Dir;
      Object : constant Session_Access := new Session;
      Image  : Wide_Wide_String := Session_Id'Wide_Wide_Image;
      PID    : Wide_Wide_String :=
        GNAT.OS_Lib.Pid_To_Integer
         (GNAT.OS_Lib.Current_Process_Id)'Wide_Wide_Image;
   begin
      PID (1) := 'P';
      Dir.Append (PID);

      Image (1) := '/';
      Object.Directory.Append (Dir);
      Object.Directory.Append (Image);
      Ada.Directories.Create_Path (Object.Directory.To_UTF_8_String);

      Object.Directory.Append ('/');
      Object.Gprbuild := Self.Gprbuild;
      Self.Map.Insert (Session_Id, Object);

      Object.Process.Set_Working_Directory (Object.Directory.To_UTF_8_String);
      Object.Process.Set_Program (Self.Driver.To_UTF_8_String);
      Object.Process.Set_Listener
        (Spawn.Processes.Process_Listener_Access (Object));
      Object.Process.Start;
      Result := Jupyter.Kernels.Session_Access (Object);
   end Create_Session;

   --------------------
   -- Error_Occurred --
   --------------------

   overriding procedure Error_Occurred
    (Self          : in out Session;
     Process_Error : Integer)
   is
      pragma Unreferenced (Process_Error);
   begin
      Self.Finished := True;
   end Error_Occurred;

   ------------------------
   -- Exception_Occurred --
   ------------------------

   overriding procedure Exception_Occurred
     (Self       : in out Session;
      Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (Occurrence);
   begin
      Self.Finished := True;
   end Exception_Occurred;

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
      Text : League.Strings.Universal_String := Code;
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

      if Self.With_Clause_Probe (Code) then
         Text.Append (" is a with-clause");
      end if;

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

   -------------------
   -- Find_Gprbuild --
   -------------------

   function Find_In_Path
     (File : String) return League.Strings.Universal_String
   is
      Found  : GNAT.OS_Lib.String_Access;
      Result : League.Strings.Universal_String;
   begin
      Found := GNAT.OS_Lib.Locate_Exec_On_Path (File);
      Result := League.Strings.From_UTF_8_String (Found.all);
      GNAT.OS_Lib.Free (Found);

      return Result;
   end Find_In_Path;

   --------------
   -- Finished --
   --------------

   overriding procedure Finished
    (Self      : in out Session;
     Exit_Code : Integer)
   is
      pragma Unreferenced (Exit_Code);
   begin
      Self.Finished := True;
   end Finished;

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
      Dir    : League.Strings.Universal_String := Top_Dir;
      PID    : Wide_Wide_String :=
        GNAT.OS_Lib.Pid_To_Integer
         (GNAT.OS_Lib.Current_Process_Id)'Wide_Wide_Image;
      Language : League.JSON.Objects.JSON_Object;
   begin
      PID (1) := 'P';
      Dir.Append (PID);
      Self.Gprbuild := Find_In_Path ("gprbuild");
      Self.Driver := Find_In_Path ("ada_driver");

      if Self.Gprbuild.Is_Empty then
         Result.Insert (+"status", -"error");
         Result.Insert (+"ename", -"NoGprbuild");
         Result.Insert (+"evalue", -"No `gprbuild` in the PATH");

         return;
      elsif Self.Driver.Is_Empty then
         Result.Insert (+"status", -"error");
         Result.Insert (+"ename", -"NoAdaDriver");
         Result.Insert (+"evalue", -"No `ada_driver` in the PATH");

         return;
      end if;

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

   -------------------------------
   -- Standard_Output_Available --
   -------------------------------

   overriding procedure Standard_Output_Available (Self : in out Session) is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Count;
      use type Jupyter.Kernels.IO_Pub_Access;
   begin
      loop
         declare
            Data : Ada.Streams.Stream_Element_Array (1 .. 512);
            Last : Ada.Streams.Stream_Element_Count;
            LF   : Ada.Streams.Stream_Element_Count := 0;
            Text : League.Strings.Universal_String;
         begin
            Self.Process.Read_Standard_Output (Data, Last);

            exit when Last < Data'First;

            if Self.IO_Pub = null then
               Self.Stdout.Append (Data (1 .. Last));
            else
               for J in reverse 1 .. Last loop
                  if Data (J) = 10 then
                     LF := J;
                     exit;
                  end if;
               end loop;

               if LF > 0 then
                  Self.Stdout.Append (Data (1 .. LF));

                  Text :=
                    League.Text_Codecs.Codec_For_Application_Locale.
                      Decode (Self.Stdout);

                  Self.IO_Pub.Stream (+"stdout", Text);

                  Self.Stdout := League.Stream_Element_Vectors.
                    To_Stream_Element_Vector (Data (LF + 1 .. Data'Last));
               else
                  Self.Stdout.Append (Data (1 .. Last));
               end if;
            end if;
         end;
      end loop;
   end Standard_Output_Available;

   -----------------------
   -- With_Clause_Probe --
   -----------------------

   function With_Clause_Probe
     (Self : aliased in out Session'Class;
      Code : League.Strings.Universal_String) return Boolean
   is
      Unit : League.Strings.Universal_String := Code;
   begin
      Unit.Append ("procedure Jupyter_With_Clause_Probe is begin null; end;");

      return Self.Compilation_Unit_Probe
        ((Name => +"jupyter_with_clause_probe.adb",
          Text => Unit));
   end With_Clause_Probe;

end Ada_Kernels;
