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
with League.Text_Codecs;

with Spawn.Processes.Monitor_Loop;

with Magics.Ls_Magic;
with Processes;

package body Ada_Kernels is
   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function "-" (Text : Wide_Wide_String)
     return League.JSON.Values.JSON_Value is
       (League.JSON.Values.To_JSON_Value (+Text));

   function "&"
     (Left : League.String_Vectors.Universal_String_Vector;
      Text : League.Strings.Universal_String)
      return League.String_Vectors.Universal_String_Vector;

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

   procedure With_Clause_Probe
     (Self : aliased in out Session'Class;
      Code : League.Strings.Universal_String;
      Text : out League.Strings.Universal_String;
      Ok   : in out Boolean);
   --  Check if given Code is a `clause` before a compilation unit

   procedure Statements_Probe
     (Self : aliased in out Session'Class;
      Code : League.Strings.Universal_String;
      Run  : League.Strings.Universal_String;
      Text : out League.Strings.Universal_String;
      Ok   : in out Boolean);
   --  Check if given Code is a statement sequence

   procedure Write_File
     (Self : aliased in out Session'Class;
      Name : League.Strings.Universal_String;
      Text : League.Strings.Universal_String);

   function Format
     (Pattern : League.Strings.Universal_String;
      Args    : League.String_Vectors.Universal_String_Vector)
      return League.Strings.Universal_String;

   function Format
     (Pattern : League.Strings.Universal_String;
      Arg     : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   type Compilation_Unit is record
      Name : League.Strings.Universal_String;
      Text : League.Strings.Universal_String;
   end record;

   function Compilation_Unit_Probe
     (Self : aliased in out Session'Class;
      Unit : Compilation_Unit) return Boolean;
   --  Check if given Unit is a valid compilation unit

   function Image (Value : Natural) return League.Strings.Universal_String;

   Top_Dir : constant League.Strings.Universal_String := +"/tmp/jupyter/";

   Line_Feed : constant Wide_Wide_Character :=
     Ada.Characters.Wide_Wide_Latin_1.LF;

   UTF_8 : constant League.Text_Codecs.Text_Codec :=
     League.Text_Codecs.Codec (+"UTF-8");

   ---------
   -- "&" --
   ---------

   function "&"
     (Left : League.String_Vectors.Universal_String_Vector;
      Text : League.Strings.Universal_String)
      return League.String_Vectors.Universal_String_Vector is
   begin
      return Result : League.String_Vectors.Universal_String_Vector := Left do
         Result.Append (Text);
      end return;
   end "&";

   ----------------------------
   -- Compilation_Unit_Probe --
   ----------------------------

   function Compilation_Unit_Probe
     (Self : aliased in out Session'Class;
      Unit : Compilation_Unit) return Boolean
   is
      use type League.Strings.Universal_String;

      Args    : League.String_Vectors.Universal_String_Vector;
      Listing : League.Strings.Universal_String;
      Errors  : League.Strings.Universal_String;
      Status  : Integer;
   begin
      Self.Write_File (Unit.Name, Unit.Text);
      Self.Write_File (+"try.gpr", +"project Try is end Try;");

      Args.Append (+"-Ptry.gpr");
      Args.Append (+"-c");
      Args.Append (+"-f");
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
      PID    : constant Natural := GNAT.OS_Lib.Pid_To_Integer
         (GNAT.OS_Lib.Current_Process_Id);
   begin
      Dir.Append (Image (PID));

      Object.Directory.Append (Dir);
      Object.Directory.Append (Image (Session_Id));
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
      use type League.Strings.Universal_String;
      Data : League.JSON.Objects.JSON_Object;
      Meta : League.JSON.Objects.JSON_Object;
      Text : League.Strings.Universal_String;
      Ok   : Boolean := False;
      Run  : constant League.Strings.Universal_String :=
        Image (Execution_Counter);
      Dir  : constant League.Strings.Universal_String :=
        Self.Directory & ".run" & Run;
   begin
      Self.IO_Pub := IO_Pub;

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

      Ada.Directories.Create_Path (Dir.To_UTF_8_String & "/.objs");
      Ada.Directories.Create_Path (Dir.To_UTF_8_String & "/.libs");

      Self.With_Clause_Probe (Code, Text, Ok);

      Self.Statements_Probe (Code, Run, Text, Ok);

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

   ------------
   -- Format --
   ------------

   function Format
     (Pattern : League.Strings.Universal_String;
      Args    : League.String_Vectors.Universal_String_Vector)
      return League.Strings.Universal_String
   is
      List : constant League.String_Vectors.Universal_String_Vector :=
        Pattern.Split ('$');
      Result : League.Strings.Universal_String := List (1);
   begin
      for J in 2 .. List.Length loop
         Result.Append (Args (J - 1));
         Result.Append (List (J));
      end loop;

      return Result;
   end Format;

   ------------
   -- Format --
   ------------

   function Format
     (Pattern : League.Strings.Universal_String;
      Arg     : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      List  : League.String_Vectors.Universal_String_Vector;
   begin
      for J in 1 .. Pattern.Count ('$') loop
         List.Append (Arg);
      end loop;

      return Format (Pattern, List);
   end Format;

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

   -----------
   -- Image --
   -----------

   function Image (Value : Natural) return League.Strings.Universal_String is
      Text : constant Wide_Wide_String := Value'Wide_Wide_Image;
   begin
      return +Text (2 .. Text'Last);
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : in out Kernel'Class;
      Error : out League.Strings.Universal_String)
   is
      Dir    : League.Strings.Universal_String := Top_Dir;
      PID    : constant Natural := GNAT.OS_Lib.Pid_To_Integer
         (GNAT.OS_Lib.Current_Process_Id);
   begin
      Dir.Append ('.');
      Dir.Append (Image (PID));
      Self.Gprbuild := Find_In_Path ("gprbuild");
      Self.Driver := Find_In_Path ("ada_driver");

      if Self.Gprbuild.Is_Empty then
         Error.Append ("No `gprbuild` in the PATH");
      elsif Self.Driver.Is_Empty then
         Error.Append ("No `ada_driver` in the PATH");

         return;
      else
         Ada.Directories.Create_Path (Dir.To_UTF_8_String);
      end if;

   end Initialize;

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
          (Line_Feed,
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

   ------------------------------
   -- Standard_Error_Available --
   ------------------------------

   overriding procedure Standard_Error_Available (Self : in out Session) is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Count;
      use type Jupyter.Kernels.IO_Pub_Access;

      procedure Append (Data : Ada.Streams.Stream_Element_Array);
      procedure On_Service_Message;

      ------------
      -- Append --
      ------------

      procedure Append (Data : Ada.Streams.Stream_Element_Array) is
      begin
         if Self.Injected then
            Self.Service.Append (Data);
         else
            Self.Stderr.Append (Data);
         end if;
      end Append;

      procedure On_Service_Message is
         Text : constant League.Strings.Universal_String :=
           UTF_8.Decode (Self.Service);
      begin
         if Text.Starts_With ("%jad_error:") then
            Self.IO_Pub.Stream (+"stderr", Text.Tail_From (12));
         elsif Text.Starts_With ("%jad_bad:") then
            Self.IO_Pub.Stream (+"stderr", Text.Tail_From (10));
         elsif Text.Starts_With ("%jad_ready") then
            Self.Ready := True;
         else
            raise Program_Error with Text.To_UTF_8_String;
         end if;

         Self.Service.Clear;
      end On_Service_Message;
   begin
      loop
         declare
            Data : Ada.Streams.Stream_Element_Array (1 .. 512);
            Last : Ada.Streams.Stream_Element_Count;
            From : Ada.Streams.Stream_Element_Count := 1;
         begin
            Self.Process.Read_Standard_Error (Data, Last);

            exit when Last < Data'First;

            for J in 1 .. Last loop
               if Data (J) = 0 then
                  Append (Data (From .. J - 1));

                  if Self.Injected then
                     On_Service_Message;
                  end if;

                  Self.Injected := not Self.Injected;
                  From := J + 1;
               end if;
            end loop;

            if From <= Last then
               Append (Data (From .. Last));
            end if;
         end;
      end loop;

      if Self.IO_Pub /= null and not Self.Stderr.Is_Empty then
         Self.IO_Pub.Stream (+"stderr", UTF_8.Decode (Self.Stderr));

         Self.Stderr.Clear;
      end if;
   end Standard_Error_Available;

   -------------------------------
   -- Standard_Output_Available --
   -------------------------------

   overriding procedure Standard_Output_Available (Self : in out Session) is
      use type Ada.Streams.Stream_Element_Count;
   begin
      loop
         declare
            Data : Ada.Streams.Stream_Element_Array (1 .. 512);
            Last : Ada.Streams.Stream_Element_Count;
         begin
            Self.Process.Read_Standard_Output (Data, Last);

            exit when Last < Data'First;

            Self.Stdout.Append (Data (1 .. Last));
         end;
      end loop;
   end Standard_Output_Available;

   ----------------------
   -- Statements_Probe --
   ----------------------

   procedure Statements_Probe
     (Self : aliased in out Session'Class;
      Code : League.Strings.Universal_String;
      Run  : League.Strings.Universal_String;
      Text : out League.Strings.Universal_String;
      Ok   : in out Boolean)
   is
      use type League.Strings.Universal_String;

      Dir     : League.Strings.Universal_String;
      Args    : League.String_Vectors.Universal_String_Vector;
      Listing : League.Strings.Universal_String;
      Errors  : League.Strings.Universal_String;
      Clauses : constant League.Strings.Universal_String :=
        Self.Clauses.Join (Line_Feed);
      Status  : Integer;
   begin
      if Ok then
         return;
      end if;

      Dir.Append (".run");
      Dir.Append (Run);
      Dir.Append ("/");
      Self.Write_File
        (Dir & Format (+"run_$.ads", Run),
         Format (+"package Run_$ is pragma Elaborate_Body; end Run_$;", Run));

      Self.Write_File
        (Dir & Format (+"run_$.adb", Run),
         Clauses &
         Format
           (+"package body Run_$ is begin $ end Run_$;",
            League.String_Vectors.Empty_Universal_String_Vector &
              Run & Code & Run));

      Args.Append (+"-Pjupyter_ada_statements.gpr");
      Args.Append (Format (+"-XRUN_DIR=$", Self.Directory & Dir));
      Args.Append (Format (+"-XEXECUTION_COUNTER=$", Run));

      Processes.Run
        (Program   => Self.Gprbuild,
         Arguments => Args,
         Directory => Self.Directory,
         Output    => Listing,
         Errors    => Errors,
         Status    => Status);

      Ok := Status = 0;

      if Ok then
         declare
            Load   : League.Strings.Universal_String;
            Ignore : Ada.Streams.Stream_Element_Offset;
            Codec  : constant League.Text_Codecs.Text_Codec :=
              League.Text_Codecs.Codec (+"UTF-8");
         begin
            Load.Append ("Load ");
            Load.Append (Dir);
            Load.Append ("/.libs/libjas");
            Load.Append (Run);
            Load.Append (".so");
            Load.Append (Line_Feed);
            Self.Process.Write_Standard_Input
              (Codec.Encode (Load).To_Stream_Element_Array, Ignore);
            Self.Ready := False;

            while not Self.Ready loop
               Spawn.Processes.Monitor_Loop (Timeout => 50);
            end loop;

            Text := UTF_8.Decode (Self.Stdout);
            Self.Stdout.Clear;
         end;
      end if;
   end Statements_Probe;

   -----------------------
   -- With_Clause_Probe --
   -----------------------

   procedure With_Clause_Probe
     (Self : aliased in out Session'Class;
      Code : League.Strings.Universal_String;
      Text : out League.Strings.Universal_String;
      Ok   : in out Boolean)
   is
      Unit : League.Strings.Universal_String := Code;
   begin
      if Ok then
         return;
      end if;

      Unit.Append ("procedure Jupyter_With_Clause_Probe is begin null; end;");

      Ok := Self.Compilation_Unit_Probe
        ((Name => +"jupyter_with_clause_probe.adb",
          Text => Unit));

      if Ok then
         Self.Clauses.Append
           (Code.Split (Line_Feed, League.Strings.Skip_Empty));
         Text.Append (Code);
         Text.Append (" is with-clause!");
      end if;
   end With_Clause_Probe;

   ----------------
   -- Write_File --
   ----------------

   procedure Write_File
     (Self : aliased in out Session'Class;
      Name : League.Strings.Universal_String;
      Text : League.Strings.Universal_String)
   is
      use type League.Strings.Universal_String;
      Output  : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Ada.Wide_Wide_Text_IO.Create
        (Output,
         Name => League.Strings.To_UTF_8_String (Self.Directory & Name),
         Form => "WCEM=8");

      Ada.Wide_Wide_Text_IO.Put_Line (Output, Text.To_Wide_Wide_String);

      Ada.Wide_Wide_Text_IO.Close (Output);
   end Write_File;

end Ada_Kernels;
