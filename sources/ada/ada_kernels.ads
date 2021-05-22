--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Containers.Hashed_Maps;
with Ada.Exceptions;
with Ada.Wide_Wide_Text_IO;

with League.JSON.Objects;
with League.Stream_Element_Vectors;
with League.String_Vectors;
with League.Strings;

with Spawn.Processes;

with Jupyter.Kernels;
with Processes;

package Ada_Kernels is

   type Kernel is limited new Jupyter.Kernels.Kernel with private;

   procedure Initialize
     (Self    : in out Kernel'Class;
      Top_Dir : League.Strings.Universal_String;
      Error   : out League.Strings.Universal_String);

private

   type Gprbuild_Options is record
      Path  : League.Strings.Universal_String;
      Gargs : League.String_Vectors.Universal_String_Vector;
      Cargs : League.String_Vectors.Universal_String_Vector;
      Largs : League.String_Vectors.Universal_String_Vector;
      Bargs : League.String_Vectors.Universal_String_Vector;
   end record;

   procedure Append
     (Args   : in out League.String_Vectors.Universal_String_Vector;
      Option : Gprbuild_Options);

   type Session is limited new Jupyter.Kernels.Session
     and Spawn.Processes.Process_Listener
   with record
      Gprbuild  : Gprbuild_Options;
      Gnatchop  : League.Strings.Universal_String;
      ALR       : League.Strings.Universal_String;
      Process   : Spawn.Processes.Process;
      Directory : League.Strings.Universal_String;
      --  Each session has its own directory
      IO_Pub    : Jupyter.Kernels.IO_Pub_Access;
      Stdout    : League.Stream_Element_Vectors.Stream_Element_Vector;
      Stderr    : League.Stream_Element_Vectors.Stream_Element_Vector;
      Service   : League.Stream_Element_Vectors.Stream_Element_Vector;
      Injected  : Boolean := False;
      Finished  : Boolean := True;
      Ready     : Boolean := True;  --  Driver's ready to get next Command
      Clauses   : League.String_Vectors.Universal_String_Vector;
      Runs      : League.String_Vectors.Universal_String_Vector;
      Build_Env : Processes.Environment;
      Trace     : Ada.Wide_Wide_Text_IO.File_Type;
   end record;

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
      Error             : in out Jupyter.Kernels.Execution_Error);

   overriding procedure Standard_Output_Available (Self : in out Session);
   overriding procedure Standard_Error_Available (Self : in out Session);

   overriding procedure Finished
    (Self      : in out Session;
     Exit_Code : Integer);

   overriding procedure Error_Occurred
    (Self          : in out Session;
     Process_Error : Integer);

   overriding procedure Exception_Occurred
     (Self       : in out Session;
      Occurrence : Ada.Exceptions.Exception_Occurrence);

   type Session_Access is access all Session;

   function Hash (Value : Positive) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (Value));

   package Session_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Positive,
      Element_Type    => Session_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Kernel is limited new Jupyter.Kernels.Kernel with record
      Top_Dir  : League.Strings.Universal_String;
      Gprbuild : League.Strings.Universal_String;
      Gnatchop : League.Strings.Universal_String;
      ALR      : League.Strings.Universal_String;
      Driver   : League.Strings.Universal_String;
      Map      : Session_Maps.Map;
      Last_Id  : Natural := 0;
   end record;

   overriding procedure Kernel_Info
     (Self   : aliased in out Kernel;
      Result : out League.JSON.Objects.JSON_Object);

   overriding procedure Create_Session
     (Self       : aliased in out Kernel;
      Session_Id : Positive;
      Result     : out Jupyter.Kernels.Session_Access);

   overriding function Get_Session
     (Self       : aliased in out Kernel;
      Session_Id : Positive) return Jupyter.Kernels.Session_Access;

end Ada_Kernels;
