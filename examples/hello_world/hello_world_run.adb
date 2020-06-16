--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Application;
with League.String_Vectors;

with Hello_World;
with Jupyter.Start_Kernel;

procedure Hello_World_Run is
   Kernel : Hello_World.Kernel;

   Args : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;
begin
   Jupyter.Start_Kernel (Kernel, Args (1));
end Hello_World_Run;
