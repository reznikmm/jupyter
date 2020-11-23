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

with Magics.ALR;
with Magics.Gprbuild_Options;
with Magics.Ls_Magic;
with Magics.Output;
with Magics.Write_File;

with Ada_Kernels.Configuration;

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

   procedure Parse_Block_Magic
     (Code : League.Strings.Universal_String;
      Args : out League.String_Vectors.Universal_String_Vector;
      Text : out League.Strings.Universal_String);
   --  Extract block magic from the first line of the Code into Args and return
   --  rest of the Code in Text.

   function Find_In_Path
     (File : String) return League.Strings.Universal_String;

   procedure Execute_Magic
     (Self              : aliased in out Session'Class;
      IO_Pub            : not null Jupyter.Kernels.IO_Pub_Access;
      Execution_Counter : Positive;
      Magic             : League.String_Vectors.Universal_String_Vector;
      Silent            : Boolean;
      Error             : in out Jupyter.Kernels.Execution_Error;
      Block             : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String);
   --  Execute one line magic command.

   procedure With_Clause_Probe
     (Self  : aliased in out Session'Class;
      Code  : League.Strings.Universal_String;
      Dir   : League.Strings.Universal_String;
      Run   : League.Strings.Universal_String;
      Error : out League.Strings.Universal_String);
   --  Check if given Code is a `clause` before a compilation unit

   procedure Launch_Cell
     (Self  : in out Session'Class;
      GPR   : League.Strings.Universal_String;
      Lib   : League.Strings.Universal_String;
      Text  : out League.Strings.Universal_String;
      Error : out League.Strings.Universal_String);
   --  Make a Lib library using given GPR and load it into driver. Return
   --  Error and Text.

   procedure Statements_Probe
     (Self  : aliased in out Session'Class;
      Code  : League.Strings.Universal_String;
      Dir   : League.Strings.Universal_String;
      Run   : League.Strings.Universal_String;
      Text  : out League.Strings.Universal_String;
      Error : out League.Strings.Universal_String);
   --  Check if given Code is a statement sequence

   procedure Declarations_Probe
     (Self  : aliased in out Session'Class;
      Code  : League.Strings.Universal_String;
      Dir   : League.Strings.Universal_String;
      Run   : League.Strings.Universal_String;
      Text  : out League.Strings.Universal_String;
      Error : out League.Strings.Universal_String);
   --  Check if given Code is a statement sequence

   procedure Compilation_Unit_Probe
     (Self  : aliased in out Session'Class;
      Code  : League.Strings.Universal_String;
      Dir   : League.Strings.Universal_String;
      Run   : League.Strings.Universal_String;
      Text  : out League.Strings.Universal_String;
      Error : out League.Strings.Universal_String);
   --  Check if given Code is a compilation unit sequence

   procedure Read_File
     (Name : League.Strings.Universal_String;
      Text : out League.Strings.Universal_String);

   procedure Write_File
     (Name : League.Strings.Universal_String;
      Text : League.Strings.Universal_String);

   function Format
     (Pattern : League.Strings.Universal_String;
      Args    : League.String_Vectors.Universal_String_Vector)
      return League.Strings.Universal_String;

   function Format
     (Pattern : League.Strings.Universal_String;
      Arg     : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Format
     (Pattern : League.Strings.Universal_String;
      Arg_1   : League.Strings.Universal_String;
      Arg_2   : League.Strings.Universal_String;
      Arg_3   : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String;
      Arg_4   : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String;
      Arg_5   : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
      return League.Strings.Universal_String;

   function Image (Value : Natural) return League.Strings.Universal_String;

   type Cell_Kind is
     (With_Cell, Execute_Cell, Run_Cell, Run_Cell_Spec);

   procedure Create_Project_File
     (Self      : aliased in out Session'Class;
      Dir       : League.Strings.Universal_String;
      Run       : League.Strings.Universal_String;
      Cell_Kind : Ada_Kernels.Cell_Kind;
      Result    : out League.Strings.Universal_String);

   function With_Runs
     (Self : Session'Class) return League.Strings.Universal_String;
   --  For each post run return `with Run_$; use Run_$;` clauses

   function To_Spec (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String;
   --  Turn Text of a subprogram body return corresponding specification

   Line_Feed : constant Wide_Wide_Character :=
     Ada.Characters.Wide_Wide_Latin_1.LF;

   UTF_8 : constant League.Text_Codecs.Text_Codec :=
     League.Text_Codecs.Codec (+"UTF-8");

   SO_Extension : Wide_Wide_String renames
     Ada_Kernels.Configuration.Shared_Library_Suffix;

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

   ------------
   -- Append --
   ------------

   procedure Append
     (Args   : in out League.String_Vectors.Universal_String_Vector;
      Option : Gprbuild_Options)
   is
      procedure Append_Section
        (Name : Wide_Wide_String;
         Value : League.String_Vectors.Universal_String_Vector);

      procedure Append_Section
        (Name : Wide_Wide_String;
         Value : League.String_Vectors.Universal_String_Vector) is
      begin
         if not Value.Is_Empty then
            Args.Append (+Name);
            Args.Append (Value);
         end if;
      end Append_Section;
   begin
      Args.Append (Option.Gargs);
      Append_Section ("-cargs", Option.Cargs);
      Append_Section ("-largs", Option.Largs);
      Append_Section ("-bargs", Option.Bargs);
   end Append;

   ----------------------------
   -- Compilation_Unit_Probe --
   ----------------------------

   procedure Compilation_Unit_Probe
     (Self  : aliased in out Session'Class;
      Code  : League.Strings.Universal_String;
      Dir   : League.Strings.Universal_String;
      Run   : League.Strings.Universal_String;
      Text  : out League.Strings.Universal_String;
      Error : out League.Strings.Universal_String)
   is
      pragma Unreferenced (Text);
      use type League.Strings.Universal_String;

      GPR    : League.Strings.Universal_String;
      Args   : League.String_Vectors.Universal_String_Vector;
      Output : League.Strings.Universal_String;
      Errors : League.Strings.Universal_String;
      Name   : League.Strings.Universal_String;
      Value  : League.Strings.Universal_String;
      Spec   : constant League.Strings.Universal_String :=
        Dir & Format (+"run_$.ads", Run);
      Impl   : constant League.Strings.Universal_String :=
        Dir & Format (+"run_$.adb", Run);
      Status : Integer;
      Names  : League.String_Vectors.Universal_String_Vector;
      Clauses : constant League.Strings.Universal_String :=
        Self.Clauses.Join (Line_Feed);
      Has_Spec : Boolean := False;
      Has_Body : Boolean := False;
   begin
      Write_File (Dir & "code", Code);
      Args.Append (+"-w");
      Args.Append (Dir & "code");
      Args.Append (Dir);

      Processes.Run
        (Program   => Self.Gnatchop,
         Arguments => Args,
         Env       => Self.Build_Env,
         Directory => Dir,
         Output    => Output,
         Errors    => Errors,
         Status    => Status);

      Names := Output.Split (Line_Feed);

      for J in 1 .. Names.Length loop
         Name := Names (J);

         if Name.Starts_With ("   ") then
            Name := Name.Tail_From (4);
            Read_File (Name, Value);
            Ada.Directories.Delete_File (Name.To_UTF_8_String);

            if Name.Ends_With (".ads") then
               Write_File
                 (Spec,
                  Clauses &
                    Self.With_Runs &
                    Format (+"package Run_$ is $ end;", Run, Value));

               Has_Spec := True;
            else
               if not Has_Spec then
                  Write_File
                    (Spec,
                     Clauses &
                       Self.With_Runs &
                       Format (+"package Run_$ is $ end;",
                               Run, To_Spec (Value)));
               end if;

               Write_File
                 (Impl,
                  Format (+"package body Run_$ is $ end;", Run, Value));

               Has_Body := True;
            end if;
         end if;
      end loop;

      if Has_Body then
         Self.Create_Project_File (Dir, Run, Run_Cell, GPR);
      elsif Has_Spec then
         Self.Create_Project_File (Dir, Run, Run_Cell_Spec, GPR);
      else
         Error := Errors;
         return;
      end if;

      Self.Launch_Cell
        (GPR,
         Format (+"$.libs/libjasrun_$" & SO_Extension, Dir, Run),
         Text,
         Error);

      if Error.Is_Empty then
         Self.Runs.Append (Run);
      end if;
   end Compilation_Unit_Probe;

   -------------------------
   -- Create_Project_File --
   -------------------------

   procedure Create_Project_File
     (Self      : aliased in out Session'Class;
      Dir       : League.Strings.Universal_String;
      Run       : League.Strings.Universal_String;
      Cell_Kind : Ada_Kernels.Cell_Kind;
      Result    : out League.Strings.Universal_String)
   is
      use type League.Strings.Universal_String;
      Lines  : League.String_Vectors.Universal_String_Vector;
      Kind   : League.Strings.Universal_String := +Cell_Kind'Wide_Wide_Image;
      Name   : League.Strings.Universal_String;
      Src    : League.Strings.Universal_String;
      Comp   : League.Strings.Universal_String;
      Bind   : League.Strings.Universal_String;
   begin
      case Cell_Kind is
         when With_Cell =>
            Src := +"Unit & "".adb""";
            Kind := +"with";
         when Execute_Cell =>
            Src := +"Unit & "".ads"", Unit & "".adb""";
            Kind := +"execute";
         when Run_Cell =>
            Src := +"Unit & "".ads"", Unit & "".adb""";
            Kind := +"run";
         when Run_Cell_Spec =>
            Src := +"Unit & "".ads""";
            Kind := +"run";
      end case;

      if not Self.ALR.Is_Empty then
         Lines.Append (+"with ""../notebook.gpr"";");
      end if;

      for J in 1 .. Self.Runs.Length loop
         Lines := Lines
           & Format (+"with ""../.run$/run_$.gpr"";", Self.Runs (J));
      end loop;

      Comp :=
        +"""-gnatW8"", ""-gnatVa"", ""-fstack-check"", ""-gnatv"", ""-g""";
      Bind := +"""-W8"", ""-E""";
      Name := Format (+"$_$", Kind, Run);
      Result := Dir & Name & ".gpr";
      Lines := Lines
        & Format (+"project $ is", Name)
        & Format (+"   Unit := ""$"";", Name)
        & Format (+"   for Source_Files use ($);", Src)
        & Format (+"   for Object_Dir use ""$.objs"";", Dir)
        & Format (+"   for Library_Dir use ""$.libs"";", Dir)
        & (+"   for Library_Kind use ""dynamic"";")
        & (+"   for Library_Interface use (Unit);")
        & Format (+"   for Library_Name use ""jas$"";", Name)
        & (+"   package Compiler is")
        & Format (+"      for Default_Switches (""Ada"") use ($);", Comp)
        & (+"   end Compiler;")
        & (+"   package Binder is")
        & Format (+"      for Switches (""Ada"") use ($);", Bind)
        & (+"   end Binder;")
        & Format (+"end $;", Name);

      Write_File (Result, Lines.Join (Line_Feed));
   end Create_Project_File;

   --------------------
   -- Create_Session --
   --------------------

   overriding procedure Create_Session
     (Self       : aliased in out Kernel;
      Session_Id : Positive;
      Result     : out Jupyter.Kernels.Session_Access)
   is
      use type League.Strings.Universal_String;

      procedure ALR_Init (Dir : League.Strings.Universal_String);

      procedure ALR_Init (Dir : League.Strings.Universal_String) is
         Args   : League.String_Vectors.Universal_String_Vector;
         Output : League.Strings.Universal_String;
         Errors : League.Strings.Universal_String;
         Status : Integer;
      begin
         Args.Append (+"--non-interactive");
         Args.Append (+"--no-tty");
         Args.Append (+"--no-color");
         Args.Append (+"init");
         Args.Append (+"--in-place");
         Args.Append (+"--no-skel");
         Args.Append (+"--lib");
         Args.Append (+"notebook");

         Processes.Run
           (Program   => Self.ALR,
            Arguments => Args,
            Directory => Dir,
            Output    => Output,
            Errors    => Errors,
            Status    => Status);

         if Status /= 0 then
            Ada.Wide_Wide_Text_IO.Put_Line
              (Ada.Wide_Wide_Text_IO.Standard_Error,
               Output.To_Wide_Wide_String);

            Ada.Wide_Wide_Text_IO.Put_Line
              (Ada.Wide_Wide_Text_IO.Standard_Error,
               Errors.To_Wide_Wide_String);
         end if;

         pragma Assert (Status = 0);
      end ALR_Init;

      Dir    : League.Strings.Universal_String := Self.Top_Dir;
      Object : constant Session_Access := new Session;
      PID    : constant Natural := GNAT.OS_Lib.Pid_To_Integer
         (GNAT.OS_Lib.Current_Process_Id);
   begin
      Dir.Append (Image (PID));

      Object.Directory.Append (Dir);
      Object.Directory.Append (Image (Session_Id));
      Ada.Directories.Create_Path (Object.Directory.To_UTF_8_String);

      Object.Directory.Append ('/');
      Object.Gprbuild.Path := Self.Gprbuild;
      Object.Gnatchop := Self.Gnatchop;
      Object.ALR := Self.ALR;
      Self.Map.Insert (Session_Id, Object);

      if not Self.ALR.Is_Empty then
         ALR_Init (Object.Directory);

         Write_File
           (Object.Directory & "notebook.gpr",
            +"abstract project Notebook is end Notebook;");
      end if;

      Object.Process.Set_Working_Directory (Object.Directory.To_UTF_8_String);
      Object.Process.Set_Program (Self.Driver.To_UTF_8_String);
      Object.Process.Set_Listener
        (Spawn.Processes.Process_Listener_Access (Object));
      Object.Process.Start;
      Result := Jupyter.Kernels.Session_Access (Object);
   end Create_Session;

   ------------------------
   -- Declarations_Probe --
   ------------------------

   procedure Declarations_Probe
     (Self  : aliased in out Session'Class;
      Code  : League.Strings.Universal_String;
      Dir   : League.Strings.Universal_String;
      Run   : League.Strings.Universal_String;
      Text  : out League.Strings.Universal_String;
      Error : out League.Strings.Universal_String)
   is
      use type League.Strings.Universal_String;

      GPR     : League.Strings.Universal_String;
      Clauses : constant League.Strings.Universal_String :=
        Self.Clauses.Join (Line_Feed);
   begin
      Write_File
        (Dir & Format (+"run_$.ads", Run),
         Clauses &
         Self.With_Runs &
         Format (+"package Run_$ is $ end;", Run, Code));

      Self.Create_Project_File (Dir, Run, Run_Cell_Spec, GPR);

      Self.Launch_Cell
        (GPR,
         Format (+"$.libs/libjasrun_$" & SO_Extension, Dir, Run),
         Text,
         Error);

      if Error.Is_Empty then
         Self.Runs.Append (Run);
      end if;
   end Declarations_Probe;

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
      With_Error : League.Strings.Universal_String;
      Stmt_Error : League.Strings.Universal_String;
      Decl_Error : League.Strings.Universal_String;
      Unit_Error : League.Strings.Universal_String;
      Data : League.JSON.Objects.JSON_Object;
      Meta : League.JSON.Objects.JSON_Object;
      Text : League.Strings.Universal_String;
      Run  : constant League.Strings.Universal_String :=
        Image (Execution_Counter);
      Dir  : constant League.Strings.Universal_String :=
        Self.Directory & ".run" & Run & "/";
   begin
      Self.IO_Pub := IO_Pub;

      declare
         Magic : League.String_Vectors.Universal_String_Vector;
         Block : League.Strings.Universal_String;
      begin
         Parse_Line_Magic (Code, Magic);

         if Magic.Length >= 1 then
            Self.Execute_Magic
              (IO_Pub, Execution_Counter, Magic, Silent, Error);
            return;
         end if;

         Parse_Block_Magic (Code, Magic, Block);

         if Magic.Length >= 1 then
            Self.Execute_Magic
              (IO_Pub, Execution_Counter, Magic, Silent, Error, Block);
            return;
         end if;
      end;

      Ada.Directories.Create_Path (Dir.To_UTF_8_String & "/.objs");
      Ada.Directories.Create_Path (Dir.To_UTF_8_String & "/.libs");

      Self.With_Clause_Probe (Code, Dir, Run, With_Error);
      if not With_Error.Is_Empty then
         Self.Statements_Probe (Code, Dir, Run, Text, Stmt_Error);

         if not Stmt_Error.Is_Empty then
            Self.Declarations_Probe (Code, Dir, Run, Text, Decl_Error);

            if not Decl_Error.Is_Empty then
               Self.Compilation_Unit_Probe (Code, Dir, Run, Text, Unit_Error);
            end if;
         end if;
      end if;

      Expression_Values := League.JSON.Objects.Empty_JSON_Object;

      if not Unit_Error.Is_Empty then
         Text := +"Unable to process. Program state doesn't change!";
         Text.Append (Line_Feed);
         Text.Append (Line_Feed);
         Text.Append ("Errors for context clauses probe:");
         Text.Append (Line_Feed);
         Text.Append (With_Error);
         Text.Append (Line_Feed);
         Text.Append (Line_Feed);
         Text.Append ("Errors for statements probe:");
         Text.Append (Line_Feed);
         Text.Append (Stmt_Error);
         Text.Append (Line_Feed);
         Text.Append (Line_Feed);
         Text.Append ("Errors for basic declarative items probe:");
         Text.Append (Line_Feed);
         Text.Append (Decl_Error);
         Text.Append (Line_Feed);
         Text.Append (Line_Feed);
         Text.Append ("Errors for compilation unit probe:");
         Text.Append (Line_Feed);
         Text.Append (Unit_Error);
         Text.Append (Line_Feed);

         IO_Pub.Stream
           (Name => +"stderr",
            Text => Text);

         Error.Name := +"CompilationFailed";
         Error.Value := Text;
      elsif not Silent and not Text.Is_Empty then
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
      Error             : in out Jupyter.Kernels.Execution_Error;
      Block             : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
   is
      pragma Unreferenced (Execution_Counter);
      use type League.Strings.Universal_String;


      Info  : League.Strings.Universal_String;
      First : League.Strings.Universal_String := Magic (1);
      Help  : constant Boolean := First.Ends_With ("?");
   begin
      if Help then
         First := First.Head_To (First.Length - 1);  --  String '?' if any
      end if;

      if First = +"%alr" then
         if Help then
            Info := (First & " {options}")
              & Line_Feed & Line_Feed
              & "Execute alr {options} to manage project dependencies.";
         else
            Magics.ALR
              (IO_Pub,
               Self.ALR,
               Self.Directory,
               Magic.Slice (2, Magic.Length),
               Self.Build_Env,
               Silent);
         end if;
      elsif First = +"%lsmagic" then
         if Help then
            Info := +"List currently available magic functions.";
         else
            Magics.Ls_Magic (IO_Pub, Silent);
         end if;
      elsif First.To_Wide_Wide_String in
        "%gargs" | "%cargs" | "%largs" | "%bargs"
      then
         if Help then
            Info := (First & " {options}")
              & Line_Feed & Line_Feed
              & "Setup additional options for gprbuild in a -"
              & First.Tail_From (2) & " section or reset if no option given.";
         elsif First.Starts_With ("%g") then
            Magics.Gprbuild_Options
              (IO_Pub,
               Silent,
               Self.Gprbuild.Gargs,
               +"-gargs",
               Magic.Slice (2, Magic.Length));
         elsif First.Starts_With ("%c") then
            Magics.Gprbuild_Options
              (IO_Pub,
               Silent,
               Self.Gprbuild.Cargs,
               +"-cargs",
               Magic.Slice (2, Magic.Length));
         elsif First.Starts_With ("%l") then
            Magics.Gprbuild_Options
              (IO_Pub,
               Silent,
               Self.Gprbuild.Largs,
               +"-largs",
               Magic.Slice (2, Magic.Length));
         elsif First.Starts_With ("%b") then
            Magics.Gprbuild_Options
              (IO_Pub,
               Silent,
               Self.Gprbuild.Bargs,
               +"-bargs",
               Magic.Slice (2, Magic.Length));
         else
            null;
         end if;
      elsif First = +"%%output" then
         if Help then
            Info := (+"%%output MIME-type")
              & Line_Feed & Line_Feed
              & "Render the cell as a content of the given MIME-type.";
         elsif Magic.Length = 2 then
            Magics.Output (IO_Pub, Magic (2), Block, Silent);
         else
            Error.Name := +"UsageError";
            Error.Value := +"Run: %%output MIME-type.";

            IO_Pub.Stream
              (Name => +"stderr",
               Text => Error.Name & ": " & Error.Value);
         end if;
      elsif First = +"%%writefile" then
         if Help then
            Info := (+"%%writefile filename")
              & Line_Feed & Line_Feed
              & "Write the contents of the cell to a file.";
         elsif Magic.Length = 2 then
            Magics.Write_File (IO_Pub, Magic (2), Block, Silent);
         else
            Error.Name := +"UsageError";
            Error.Value := +"Run: %%writefile <filename>.";

            IO_Pub.Stream
              (Name => +"stderr",
               Text => Error.Name & ": " & Error.Value);
         end if;
      else
         Error.Name := +"UsageError";
         Error.Value := "Magic function `" & First & "` not found."
           & Line_Feed & Line_Feed
           & "Use %lsmagic to list available magic functions.";

         IO_Pub.Stream
           (Name => +"stderr",
            Text => Error.Name & ": " & Error.Value);
      end if;

      if not Info.Is_Empty then
         IO_Pub.Stream (Name => +"stderr", Text => Info);
      end if;
   end Execute_Magic;

   -------------------
   -- Find_Gprbuild --
   -------------------

   function Find_In_Path
     (File : String) return League.Strings.Universal_String
   is
      use type GNAT.OS_Lib.String_Access;

      Found  : GNAT.OS_Lib.String_Access;
      Result : League.Strings.Universal_String;
   begin
      Found := GNAT.OS_Lib.Locate_Exec_On_Path (File);

      if Found /= null then
         Result := League.Strings.From_UTF_8_String (Found.all);
         GNAT.OS_Lib.Free (Found);
      end if;

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

   ------------
   -- Format --
   ------------

   function Format
     (Pattern : League.Strings.Universal_String;
      Arg_1   : League.Strings.Universal_String;
      Arg_2   : League.Strings.Universal_String;
      Arg_3   : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String;
      Arg_4   : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String;
      Arg_5   : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
      return League.Strings.Universal_String
   is
      List  : League.String_Vectors.Universal_String_Vector;
   begin
      List.Append (Arg_1);
      List.Append (Arg_2);
      List.Append (Arg_3);
      List.Append (Arg_4);
      List.Append (Arg_5);
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
     (Self    : in out Kernel'Class;
      Top_Dir : League.Strings.Universal_String;
      Error   : out League.Strings.Universal_String) is
   begin
      Self.Top_Dir := Top_Dir;
      Self.Gprbuild := Find_In_Path ("gprbuild");
      Self.Gnatchop := Find_In_Path ("gnatchop");
      Self.ALR := Find_In_Path ("alr");
      Self.Driver := Find_In_Path ("ada_driver");

      if Self.Gprbuild.Is_Empty then
         Error.Append ("No `gprbuild` in the PATH");
      elsif Self.Gnatchop.Is_Empty then
         Error.Append ("No `gnatchop` in the PATH");
      elsif Self.Driver.Is_Empty then
         Error.Append ("No `ada_driver` in the PATH");
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
      Language.Insert (+"name", -"Ada");
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

   -----------------
   -- Launch_Cell --
   -----------------

   procedure Launch_Cell
     (Self  : in out Session'Class;
      GPR   : League.Strings.Universal_String;
      Lib   : League.Strings.Universal_String;
      Text  : out League.Strings.Universal_String;
      Error : out League.Strings.Universal_String)
   is
      use type League.Strings.Universal_String;
      Args    : League.String_Vectors.Universal_String_Vector;
      Listing : League.Strings.Universal_String;
      Errors  : League.Strings.Universal_String;
      Status  : Integer;
   begin
      Args.Append (+"-p");
      Args.Append ("-P" & GPR);
      Append (Args, Self.Gprbuild);  --  Append custom options

      Processes.Run
        (Program   => Self.Gprbuild.Path,
         Arguments => Args,
         Env       => Self.Build_Env,
         Directory => Self.Directory,
         Output    => Listing,
         Errors    => Errors,
         Status    => Status);

      if Status = 0 then
         declare
            Load   : League.Strings.Universal_String;
            Ignore : Ada.Streams.Stream_Element_Offset;
            Codec  : constant League.Text_Codecs.Text_Codec :=
              League.Text_Codecs.Codec (+"UTF-8");
         begin
            Load := Format (+"Load $", Lib);
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
      else
         Error := Listing;
      end if;
   end Launch_Cell;

   -----------------------
   -- Parse_Block_Magic --
   -----------------------

   procedure Parse_Block_Magic
     (Code : League.Strings.Universal_String;
      Args : out League.String_Vectors.Universal_String_Vector;
      Text : out League.Strings.Universal_String)
   is
      Pos : constant Natural := Code.Index (Line_Feed);
   begin
      if Pos = 0 then
         Args := Code.Split (' ');
         Text.Clear;
      else
         Args := Code.Head_To (Pos - 1).Split (' ');
         Text := Code.Tail_From (Pos + 1);
      end if;

      if Args.Length >= 1 then
         if not Args (1).Starts_With ("%%") then
            Args.Clear;
         end if;
      end if;
   end Parse_Block_Magic;

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

   ---------------
   -- Read_File --
   ---------------

   procedure Read_File
     (Name : League.Strings.Universal_String;
      Text : out League.Strings.Universal_String)
   is
      Input  : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Ada.Wide_Wide_Text_IO.Open
        (Input,
         Mode => Ada.Wide_Wide_Text_IO.In_File,
         Name => Name.To_UTF_8_String,
         Form => "WCEM=8");

      Text.Clear;

      while not Ada.Wide_Wide_Text_IO.End_Of_File (Input) loop
         Text.Append (Ada.Wide_Wide_Text_IO.Get_Line (Input));
         Text.Append (Line_Feed);
      end loop;

      Ada.Wide_Wide_Text_IO.Close (Input);
   end Read_File;

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
     (Self  : aliased in out Session'Class;
      Code  : League.Strings.Universal_String;
      Dir   : League.Strings.Universal_String;
      Run   : League.Strings.Universal_String;
      Text  : out League.Strings.Universal_String;
      Error : out League.Strings.Universal_String)
   is
      use type League.Strings.Universal_String;

      GPR     : League.Strings.Universal_String;
      Clauses : constant League.Strings.Universal_String :=
        Self.Clauses.Join (Line_Feed);
   begin
      Write_File
        (Dir & Format (+"execute_$.ads", Run),
         Format
           (+"package Execute_$ is pragma Elaborate_Body; end;",
            Run));

      Write_File
        (Dir & Format (+"execute_$.adb", Run),
         Clauses &
         Self.With_Runs &
         Format (+"package body Execute_$ is begin $ end;", Run, Code));

      Self.Create_Project_File (Dir, Run, Execute_Cell, GPR);

      Self.Launch_Cell
        (GPR,
         Format (+"$.libs/libjasexecute_$" & SO_Extension, Dir, Run),
         Text,
         Error);
   end Statements_Probe;

   -------------
   -- To_Spec --
   -------------

   function To_Spec (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
      Line  : League.Strings.Universal_String;
      Lines : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split (Line_Feed);
      Result : League.String_Vectors.Universal_String_Vector;
   begin
      for J in 1 .. Lines.Length loop
         Line := Lines (J);

         if Line.Ends_With (" is") then
            Result.Append (Line.Head_To (Line.Length - 3) & ";");
            exit;
         elsif Line.Index (" is ") > 0 then
            Result.Append (Line.Head_To (Line.Index (" is ")) & ";");
            exit;
         else
            Result.Append (Line);
         end if;
      end loop;

      return Result.Join (Line_Feed);
   end To_Spec;

   -----------------------
   -- With_Clause_Probe --
   -----------------------

   procedure With_Clause_Probe
     (Self  : aliased in out Session'Class;
      Code  : League.Strings.Universal_String;
      Dir   : League.Strings.Universal_String;
      Run   : League.Strings.Universal_String;
      Error : out League.Strings.Universal_String)
   is
      use type League.Strings.Universal_String;

      GPR     : League.Strings.Universal_String;
      Args    : League.String_Vectors.Universal_String_Vector;
      Listing : League.Strings.Universal_String;
      Errors  : League.Strings.Universal_String;
      Clauses : constant League.Strings.Universal_String :=
        Self.Clauses.Join (Line_Feed);
      Status  : Integer;
   begin
      Write_File
        (Dir & Format (+"with_$.adb", Run),
         Clauses &
         Self.With_Runs &
         Code &
         Format (+"procedure With_$ is begin null; end;", Run));

      Self.Create_Project_File (Dir, Run, With_Cell, GPR);
      Args.Append (+"-p");
      Args.Append ("-P" & GPR);
      Args.Append (+"-c");  --  Just compile
      Append (Args, Self.Gprbuild);  --  Append custom options

      Processes.Run
        (Program   => Self.Gprbuild.Path,
         Arguments => Args,
         Env       => Self.Build_Env,
         Directory => Self.Directory,
         Output    => Listing,
         Errors    => Errors,
         Status    => Status);

      if Status = 0 then
         Self.Clauses.Append
           (Code.Split (Line_Feed, League.Strings.Skip_Empty));
      elsif Listing.Is_Empty then
         Error := Errors;
      else
         Error := Listing;
      end if;
   end With_Clause_Probe;

   ---------------
   -- With_Runs --
   ---------------

   function With_Runs
     (Self : Session'Class) return League.Strings.Universal_String
   is
      Result : League.String_Vectors.Universal_String_Vector;
   begin
      for J in 1 .. Self.Runs.Length loop
         Result.Append
           (Format (+"with Run_$; use Run_$;", Self.Runs (J)));
      end loop;

      return Result.Join (Line_Feed);
   end With_Runs;

   ----------------
   -- Write_File --
   ----------------

   procedure Write_File
     (Name : League.Strings.Universal_String;
      Text : League.Strings.Universal_String)
   is
      Output  : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Ada.Wide_Wide_Text_IO.Create
        (Output,
         Name => Name.To_UTF_8_String,
         Form => "WCEM=8");

      Ada.Wide_Wide_Text_IO.Put_Line (Output, Text.To_Wide_Wide_String);

      Ada.Wide_Wide_Text_IO.Close (Output);
   end Write_File;

end Ada_Kernels;
