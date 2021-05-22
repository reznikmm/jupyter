--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Wide_Wide_Text_IO;

with League.JSON.Objects;
with League.JSON.Values;
with League.Stream_Element_Vectors;
with League.String_Vectors;
with League.Text_Codecs;
with League.Regexps;

with Spawn.Processes.Monitor_Loop;
with Spawn.Processes;
with Spawn.String_Vectors;

with Jupyter.Kernels;
with Processes;

procedure Magics.ALR
  (IO_Pub : not null Jupyter.Kernels.IO_Pub_Access;
   ALR    : League.Strings.Universal_String;
   Dir    : League.Strings.Universal_String;
   Args   : League.String_Vectors.Universal_String_Vector;
   Env    : in out Processes.Environment;
   Silent : Boolean)
is

   procedure Write (Text : League.Strings.Universal_String);
   --  Write text to jupyter

   procedure Execute (Ok : in out Boolean);
   --  Execute ALR with Args in Dir

   procedure ALR_Print_Env;
   --  Execute ALR printenv

   procedure Parse_Line (Line : League.Strings.Universal_String);

   Codec : constant League.Text_Codecs.Text_Codec :=
     League.Text_Codecs.Codec_For_Application_Locale;

   Pattern : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile
       (+"^export\ ([A-Za-z0-9_\-]+)\=\""(.*)\""$");

   package Base is
      type Listener is abstract limited new Spawn.Processes.Process_Listener
        with record
         Process : Spawn.Processes.Process;
         Error   : League.Stream_Element_Vectors.Stream_Element_Vector;
         Done    : Boolean := False;
         Ok      : Boolean := False;
        end record;

      overriding procedure Standard_Input_Available (Self : in out Listener)
        is null;

      overriding procedure Started (Self : in out Listener) is null;

      overriding procedure Finished
        (Self        : in out Listener;
         Exit_Status : Spawn.Processes.Process_Exit_Status;
         Exit_Code   : Spawn.Processes.Process_Exit_Code);

      overriding procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer);

      overriding procedure Exception_Occurred
        (Self       : in out Listener;
         Occurrence : Ada.Exceptions.Exception_Occurrence);

      overriding procedure Standard_Error_Available (Self : in out Listener);

   end Base;

   package body Base is

      --------------------
      -- Error_Occurred --
      --------------------

      procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer)
      is
         pragma Unreferenced (Self);
         Result : League.Strings.Universal_String;
      begin
         Result.Append (+"Unexpected error:");
         Result.Append (+Integer'Wide_Wide_Image (Process_Error));
         Write (Result);
      end Error_Occurred;

      ------------------------
      -- Exception_Occurred --
      ------------------------

      procedure Exception_Occurred
        (Self       : in out Listener;
         Occurrence : Ada.Exceptions.Exception_Occurrence)
      is
         Result : League.Strings.Universal_String;
      begin
         Result.Append ("Exception: ");
         Result.Append
           (League.Strings.From_UTF_8_String
              (Ada.Exceptions.Exception_Name (Occurrence)));
         Result.Append (Ada.Characters.Wide_Wide_Latin_1.LF);
         Result.Append
           (League.Strings.From_UTF_8_String
              (Ada.Exceptions.Exception_Message (Occurrence)));
         Self.Done := True;
         Write (Result);
      end Exception_Occurred;

      --------------
      -- Finished --
      --------------

      procedure Finished
        (Self        : in out Listener;
         Exit_Status : Spawn.Processes.Process_Exit_Status;
         Exit_Code   : Spawn.Processes.Process_Exit_Code)
      is
         use all type Spawn.Processes.Process_Exit_Status;
         Result : League.Strings.Universal_String;
      begin
         if Exit_Status = Normal then
            Self.Ok := True;
         else
            Result.Append (+"Exit code: ");
            Result.Append (+Exit_Code'Wide_Wide_Image);
            Write (Result);
         end if;

         Self.Done := True;
      end Finished;

      ------------------------------
      -- Standard_Error_Available --
      ------------------------------

      procedure Standard_Error_Available (Self : in out Listener) is
         use type Ada.Streams.Stream_Element;
         use type Ada.Streams.Stream_Element_Count;
         Data : Ada.Streams.Stream_Element_Array (1 .. 512);
         Last : Ada.Streams.Stream_Element_Count;
         Tail : Ada.Streams.Stream_Element_Count := 0;
      begin
         loop
            Self.Process.Read_Standard_Error (Data, Last);
            exit when Last < Data'First;

            for J in reverse 1 .. Last loop
               if Data (J) <= 16#7F# then
                  Self.Error.Append (Data (1 .. J));
                  Tail := J + 1;
                  exit;
               elsif Data (J) >= 16#C0# then
                  Self.Error.Append (Data (1 .. J - 1));
                  Tail := J;
               end if;
            end loop;
         end loop;

         Write (Codec.Decode (Self.Error));
         Self.Error.Clear;
         Self.Error.Append (Data (Tail .. Last));
      end Standard_Error_Available;

   end Base;

   procedure ALR_Print_Env is

      type Listener is new Base.Listener with record
         Output : League.Stream_Element_Vectors.Stream_Element_Vector;
         Status : Integer := 0;
      end record;

      procedure Standard_Output_Available (Self : in out Listener);

      -------------------------------
      -- Standard_Output_Available --
      -------------------------------

      procedure Standard_Output_Available (Self : in out Listener) is
         use type Ada.Streams.Stream_Element;
         use type Ada.Streams.Stream_Element_Count;
         Data : Ada.Streams.Stream_Element_Array (1 .. 512);
         Last : Ada.Streams.Stream_Element_Count;
      begin
         loop
            Self.Process.Read_Standard_Output (Data, Last);

            for J in 1 .. Last loop
               if Data (J) = 16#0A# then
                  Parse_Line (Codec.Decode (Self.Output));
                  Self.Output.Clear;
               else
                  Self.Output.Append (Data (J));
               end if;
            end loop;

            exit when Last < Data'First;
         end loop;
      end Standard_Output_Available;

      Feedback : aliased Listener;
      Process  : Spawn.Processes.Process renames Feedback.Process;
      Params   : Spawn.String_Vectors.UTF_8_String_Vector;
   begin
      Params.Append ("printenv");
      Process.Set_Arguments (Params);
      Process.Set_Working_Directory (Dir.To_UTF_8_String);
      Process.Set_Program (ALR.To_UTF_8_String);
      Process.Set_Listener (Feedback'Unchecked_Access);
      Process.Start;

      while not Feedback.Done loop
         Spawn.Processes.Monitor_Loop (Timeout => 50);
      end loop;
   end ALR_Print_Env;

   procedure Execute (Ok : in out Boolean) is

      type Listener is new Base.Listener with record
         Output : League.Stream_Element_Vectors.Stream_Element_Vector;
         Write  : Boolean := True;
      end record;

      overriding procedure Standard_Output_Available (Self : in out Listener);

      overriding procedure Finished
        (Self        : in out Listener;
         Exit_Status : Spawn.Processes.Process_Exit_Status;
         Exit_Code   : Spawn.Processes.Process_Exit_Code);

      overriding procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer);

      overriding procedure Exception_Occurred
        (Self       : in out Listener;
         Occurrence : Ada.Exceptions.Exception_Occurrence);

      Codec : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec_For_Application_Locale;

      --------------------
      -- Error_Occurred --
      --------------------

      procedure Error_Occurred
        (Self          : in out Listener;
         Process_Error : Integer)
      is
         pragma Unreferenced (Self);
         Result : League.Strings.Universal_String;
      begin
         Result.Append (+"Unexpected error:");
         Result.Append (+Integer'Wide_Wide_Image (Process_Error));
         Write (Result);
      end Error_Occurred;

      ------------------------
      -- Exception_Occurred --
      ------------------------

      procedure Exception_Occurred
        (Self       : in out Listener;
         Occurrence : Ada.Exceptions.Exception_Occurrence)
      is
         Result : League.Strings.Universal_String;
      begin
         Result.Append ("Exception: ");
         Result.Append
           (League.Strings.From_UTF_8_String
              (Ada.Exceptions.Exception_Name (Occurrence)));
         Result.Append (Ada.Characters.Wide_Wide_Latin_1.LF);
         Result.Append
           (League.Strings.From_UTF_8_String
              (Ada.Exceptions.Exception_Message (Occurrence)));
         Self.Done := True;
         Write (Result);
      end Exception_Occurred;

      --------------
      -- Finished --
      --------------

      procedure Finished
        (Self        : in out Listener;
         Exit_Status : Spawn.Processes.Process_Exit_Status;
         Exit_Code   : Spawn.Processes.Process_Exit_Code)
      is
         use all type Spawn.Processes.Process_Exit_Status;
         Result : League.Strings.Universal_String;
      begin
         if Exit_Status = Normal then
            Self.Ok := True;
         else
            Result.Append (+"Exit code: ");
            Result.Append (+Exit_Code'Wide_Wide_Image);
            Write (Result);
         end if;

         Self.Done := True;
      end Finished;

      -------------------------------
      -- Standard_Output_Available --
      -------------------------------

      procedure Standard_Output_Available (Self : in out Listener) is
         use type Ada.Streams.Stream_Element;
         use type Ada.Streams.Stream_Element_Count;
         Data : Ada.Streams.Stream_Element_Array (1 .. 512);
         Last : Ada.Streams.Stream_Element_Count;
         Tail : Ada.Streams.Stream_Element_Count := 0;
      begin
         loop
            Self.Process.Read_Standard_Output (Data, Last);
            exit when Last < Data'First;

            for J in reverse 1 .. Last loop
               if Data (J) <= 16#7F# then
                  Self.Output.Append (Data (1 .. J));
                  Tail := J + 1;
                  exit;
               elsif Data (J) >= 16#C0# then
                  Self.Output.Append (Data (1 .. J - 1));
                  Tail := J;
               end if;
            end loop;
         end loop;

         Write (Codec.Decode (Self.Output));
         Self.Output.Clear;
         Self.Output.Append (Data (Tail .. Last));
      end Standard_Output_Available;

      Feedback : aliased Listener;
      Process  : Spawn.Processes.Process renames Feedback.Process;
      Params   : Spawn.String_Vectors.UTF_8_String_Vector;
   begin
      Params.Append ("--non-interactive");
      Params.Append ("--no-tty");
      Params.Append ("--no-color");

      for J in 1 .. Args.Length loop
         Params.Append (Args (J).To_UTF_8_String);
      end loop;

      Process.Set_Arguments (Params);
      Process.Set_Working_Directory (Dir.To_UTF_8_String);
      Process.Set_Program (ALR.To_UTF_8_String);
      Process.Set_Listener (Feedback'Unchecked_Access);
      Process.Start;

      while not Feedback.Done loop
         Spawn.Processes.Monitor_Loop (Timeout => 50);
      end loop;

      Ok := Feedback.Ok;
   end Execute;

   ----------------
   -- Parse_Line --
   ----------------

   procedure Parse_Line (Line : League.Strings.Universal_String) is
      Index : Natural;
      Match : constant League.Regexps.Regexp_Match :=
        Pattern.Find_Match (Line);
   begin
      if not Match.Is_Matched then
         return;
      end if;

      Index := Env.Names.Index (Match.Capture (1));

      if Index > 0 then
         Env.Values.Replace (Index, Match.Capture (2));
      else
         Env.Names.Append (Match.Capture (1));
         Env.Values.Append (Match.Capture (2));
      end if;
   end Parse_Line;

   -----------
   -- Write --
   -----------

   procedure Write (Text : League.Strings.Universal_String) is
      Data   : League.JSON.Objects.JSON_Object;
   begin
      if not Silent then
         Data.Insert (+"text/plain", League.JSON.Values.To_JSON_Value (Text));
         IO_Pub.Execute_Result
           (Data      => Data,
            Metadata  => League.JSON.Objects.Empty_JSON_Object,
            Transient => League.JSON.Objects.Empty_JSON_Object);
      end if;
   end Write;

   Ok : Boolean := False;
begin
   Execute (Ok);

   if Ok then
      --  Run `alr printenv` to update environment
      ALR_Print_Env;
      for J in 1 .. Env.Names.Length loop
         Ada.Wide_Wide_Text_IO.Put_Line
           (Ada.Wide_Wide_Text_IO.Standard_Error,
            Env.Names (J).To_Wide_Wide_String & "=" &
            Env.Values (J).To_Wide_Wide_String);
      end loop;
   end if;
end Magics.ALR;
