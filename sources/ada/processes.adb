--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Streams;
with Ada.Exceptions;

with League.Stream_Element_Vectors;
with League.Text_Codecs;

with Spawn.Processes.Monitor_Loop;
with Spawn.Processes;
with Spawn.String_Vectors;

package body Processes is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   ---------
   -- Run --
   ---------

   procedure Run
     (Program   :     League.Strings.Universal_String;
      Arguments :     League.String_Vectors.Universal_String_Vector;
      Directory :     League.Strings.Universal_String;
      Output    : out League.Strings.Universal_String;
      Errors    : out League.Strings.Universal_String;
      Status    : out Integer)
   is
      type Listener is new Spawn.Processes.Process_Listener with record
         Output : League.Stream_Element_Vectors.Stream_Element_Vector;
         Errors : League.Stream_Element_Vectors.Stream_Element_Vector;
         Status : Integer := 0;
         Done   : Boolean := False;
      end record;

      procedure Standard_Output_Available (Self : in out Listener);
      procedure Standard_Error_Available (Self : in out Listener);

      procedure Finished
        (Self      : in out Listener;
         Exit_Code : Integer);

      procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer);

      procedure Exception_Occurred
        (Self       : in out Listener;
         Occurrence : Ada.Exceptions.Exception_Occurrence);

      Process : Spawn.Processes.Process;

      -------------------------------
      -- Standard_Output_Available --
      -------------------------------

      procedure Standard_Output_Available (Self : in out Listener) is
         use type Ada.Streams.Stream_Element_Count;
         Data : Ada.Streams.Stream_Element_Array (1 .. 512);
         Last : Ada.Streams.Stream_Element_Count;
      begin
         loop
            Process.Read_Standard_Output (Data, Last);
            exit when Last < Data'First;
            Self.Output.Append (Data (1 .. Last));
         end loop;
      end Standard_Output_Available;

      ------------------------------
      -- Standard_Error_Available --
      ------------------------------

      procedure Standard_Error_Available (Self : in out Listener) is
         use type Ada.Streams.Stream_Element_Count;
         Data : Ada.Streams.Stream_Element_Array (1 .. 512);
         Last : Ada.Streams.Stream_Element_Count;
      begin
         loop
            Process.Read_Standard_Error (Data, Last);
            exit when Last < Data'First;
            Self.Errors.Append (Data (1 .. Last));
         end loop;
      end Standard_Error_Available;

      --------------
      -- Finished --
      --------------

      procedure Finished
        (Self      : in out Listener;
         Exit_Code : Integer) is
      begin
         Self.Status := Exit_Code;
         Self.Done := True;
      end Finished;

      --------------------
      -- Error_Occurred --
      --------------------

      procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer) is
         pragma Unreferenced (Self);
      begin
         Errors.Append (+"Error_Occurred");
         Self.Status := Process_Error;
         Self.Done := True;
      end Error_Occurred;

      procedure Exception_Occurred
        (Self       : in out Listener;
         Occurrence : Ada.Exceptions.Exception_Occurrence) is
      begin
         Errors.Append
           (League.Strings.From_UTF_8_String
             (Ada.Exceptions.Exception_Information (Occurrence)));

         Self.Status := -1;
         Self.Done := True;
      end Exception_Occurred;

      Codec : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec_For_Application_Locale;

      Args     : Spawn.String_Vectors.UTF_8_String_Vector;
      Feedback : aliased Listener;
   begin
      Process.Set_Program (Program.To_UTF_8_String);

      for J in 1 .. Arguments.Length loop
         Args.Append (Arguments (J).To_UTF_8_String);
      end loop;

      Process.Set_Arguments (Args);
      Process.Set_Working_Directory (Directory.To_UTF_8_String);
      Process.Set_Listener (Feedback'Unchecked_Access);
      Process.Start;

      while not Feedback.Done loop
         Spawn.Processes.Monitor_Loop (Timeout => 50);
      end loop;

      Output := Codec.Decode (Feedback.Output);
      Errors.Append (Codec.Decode (Feedback.Errors));
      Status := Feedback.Status;
   end Run;

end Processes;
