--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Application;
with League.String_Vectors;
with League.Strings;

with Ada_Kernels;
with Jupyter.Start_Kernel;

procedure Ada_Kernel is
   Kernel : Ada_Kernels.Kernel;
   Error  : League.Strings.Universal_String;
   Args   : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;
begin
   Kernel.Initialize (Error);

   if Error.Is_Empty then
      Jupyter.Start_Kernel (Kernel, Args (1));
   else
      raise Program_Error with Error.To_UTF_8_String;
   end if;
end Ada_Kernel;
