--  SPDX-FileCopyrightText: 2020-2022 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
---------------------------------------------------------------------

with Ada.Strings.UTF_Encoding;

function Load_Library
  (Name  : Ada.Strings.UTF_Encoding.UTF_8_String)
    return Ada.Strings.UTF_Encoding.UTF_8_String;
--  Load a shared library with given Name and return error or an empty
--  string if success.
