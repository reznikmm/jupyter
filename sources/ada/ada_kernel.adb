--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Application;
with League.String_Vectors;

with Ada_Kernels;
with Jupyter.Start_Kernel;

procedure Ada_Kernel is
   Kernel : Ada_Kernels.Kernel;

   Args : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;
begin
   Jupyter.Start_Kernel (Kernel, Args (1));
end Ada_Kernel;
