--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Containers.Hashed_Maps;

with League.JSON.Objects;
with League.Strings;

with Jupyter.Kernels;

package Hello_World is

   type Kernel is limited new Jupyter.Kernels.Kernel with private;

private
   type Session is limited new Jupyter.Kernels.Session with null record;

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

   type Session_Access is access all Session;

   function Hash (Value : Positive) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (Value));

   package Session_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Positive,
      Element_Type    => Session_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Kernel is limited new Jupyter.Kernels.Kernel with record
      Map     : Session_Maps.Map;
      Last_Id : Natural := 0;
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

end Hello_World;
