--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Strings;

with Jupyter.Kernels;

procedure Jupyter.Start_Kernel
  (Kernel : in out Jupyter.Kernels.Kernel'Class;
   File   : League.Strings.Universal_String);
