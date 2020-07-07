--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Wide_Wide_Text_IO;
with GNAT.SHA256;
with Interfaces.C;

with League.Text_Codecs;
with League.Holders;
with League.JSON.Documents;
with League.JSON.Objects;
with League.JSON.Values;
with League.Stream_Element_Vectors;
with League.Strings;
with League.Strings.Hash;

with ZMQ.Contexts;
with ZMQ.Messages;
with ZMQ.Sockets;
with ZMQ.Low_Level;

procedure Jupyter.Start_Kernel
  (Kernel : in out Jupyter.Kernels.Kernel'Class;
   File   : League.Strings.Universal_String)
is
   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function "-" (Text : Wide_Wide_String) return League.JSON.Values.JSON_Value;

   procedure Read_Connection_File
     (Name   : League.Strings.Universal_String;
      Result : out League.JSON.Objects.JSON_Object);

   procedure Bind
     (Ctx    : ZMQ.Contexts.Context;
      Socket : in out ZMQ.Sockets.Socket;
      CF     : League.JSON.Objects.JSON_Object;
      Port   : Wide_Wide_String;
      Kind   : ZMQ.Sockets.Socket_Type);

   type Frontend_Connection is record
      Ctx     : ZMQ.Contexts.Context;
      Key     : League.Strings.Universal_String;
      Shell   : ZMQ.Sockets.Socket;
      Stdin   : ZMQ.Sockets.Socket;
      IOPub   : ZMQ.Sockets.Socket;
      Control : ZMQ.Sockets.Socket;
      Ping    : ZMQ.Sockets.Socket;
   end record;

   package IO_Pubs is
      type IO_Pub (Up : not null access Frontend_Connection) is
        new Jupyter.Kernels.IO_Pub with
      record
         Request : League.JSON.Objects.JSON_Object;
         Id      : Positive;
         Count   : Positive;
      end record;

      overriding procedure Stream
        (Self : in out IO_Pub;
         Name : League.Strings.Universal_String;
         Text : League.Strings.Universal_String);

      overriding procedure Display_Data
        (Self      : in out IO_Pub;
         Data      : League.JSON.Objects.JSON_Object;
         Metadata  : League.JSON.Objects.JSON_Object;
         Transient : League.JSON.Objects.JSON_Object);

      overriding procedure Update_Display_Data
        (Self      : in out IO_Pub;
         Data      : League.JSON.Objects.JSON_Object;
         Metadata  : League.JSON.Objects.JSON_Object;
         Transient : League.JSON.Objects.JSON_Object);

      overriding procedure Execute_Result
        (Self      : in out IO_Pub;
         Data      : League.JSON.Objects.JSON_Object;
         Metadata  : League.JSON.Objects.JSON_Object;
         Transient : League.JSON.Objects.JSON_Object);

      overriding procedure Execute_Error
        (Self  : in out IO_Pub;
         Value : Jupyter.Kernels.Execution_Error);

      overriding procedure Clear_Output
        (Self  : in out IO_Pub;
         Wait  : Boolean);

      overriding procedure Debug_Event
        (Self      : in out IO_Pub;
         Content   : League.JSON.Objects.JSON_Object);
   end IO_Pubs;

   package Address_Lists is new Ada.Containers.Doubly_Linked_Lists
     (League.Stream_Element_Vectors.Stream_Element_Vector,
      League.Stream_Element_Vectors."=");

   function "-"
     (Text : League.Strings.Universal_String) return Address_Lists.List;

   procedure Send_Message
     (Socket  : in out ZMQ.Sockets.Socket;
      To      : Address_Lists.List;
      Key     : League.Strings.Universal_String;
      Kind    : Wide_Wide_String;
      Parent  : League.JSON.Objects.JSON_Object :=
        League.JSON.Objects.Empty_JSON_Object;
      Content : League.JSON.Objects.JSON_Object :=
        League.JSON.Objects.Empty_JSON_Object);

   package body IO_Pubs is separate;

   type Message is record
      From      : Address_Lists.List;
      Signature : League.Strings.Universal_String;
      Header    : League.JSON.Objects.JSON_Object;
      Parent    : League.JSON.Objects.JSON_Object;
      Metadata  : League.JSON.Objects.JSON_Object;
      Content   : League.JSON.Objects.JSON_Object;
   end record;

   procedure Read_Message
     (Socket : ZMQ.Sockets.Socket;
      Key    : League.Strings.Universal_String;
      Result : out Message);

   procedure Process_Shell_Message
     (Frontend : aliased in out Frontend_Connection;
      Request  : Message);

   function Has_More (Message : ZMQ.Messages.Message) return Boolean;

   type Session_Information is record
      Id    : Positive;
      Count : Positive;
   end record;

   package Session_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => League.Strings.Universal_String,
      Element_Type    => Session_Information,
      Hash            => League.Strings.Hash,
      Equivalent_Keys => League.Strings."=");

   Next_Id : Positive := 1;
   Map     : Session_Maps.Map;

   ---------
   -- "-" --
   ---------

   function "-" (Text : Wide_Wide_String)
     return League.JSON.Values.JSON_Value is
   begin
      return League.JSON.Values.To_JSON_Value (+Text);
   end "-";

   ---------
   -- "-" --
   ---------

   function "-"
     (Text : League.Strings.Universal_String) return Address_Lists.List
   is
      Codec : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec_For_Application_Locale;
   begin
      return Result : Address_Lists.List do
         Result.Append (Codec.Encode (Text));
      end return;
   end "-";

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Ctx    : ZMQ.Contexts.Context;
      Socket : in out ZMQ.Sockets.Socket;
      CF     : League.JSON.Objects.JSON_Object;
      Port   : Wide_Wide_String;
      Kind   : ZMQ.Sockets.Socket_Type)
   is
      Image : constant String := CF.Value (+Port).To_Integer'Img;
      Address : constant String :=
        CF.Value (+"transport").To_String.To_UTF_8_String &
        "://" &
        CF.Value (+"ip").To_String.To_UTF_8_String &
        ":" & Image (2 .. Image'Last);
   begin
      Socket.Initialize (Ctx, Kind);
      Socket.Bind (Address);
   end Bind;

   --------------
   -- Has_More --
   --------------

   function Has_More (Message : ZMQ.Messages.Message) return Boolean is
      use type Interfaces.C.int;
   begin
      return ZMQ.Low_Level.zmq_msg_more (Message.GetImpl) /= 0;
   end Has_More;

   -------------
   -- Process --
   -------------

   procedure Process_Shell_Message
     (Frontend : aliased in out Frontend_Connection;
      Request  : Message)
   is
      use type League.Strings.Universal_String;
      Topic  : League.Strings.Universal_String;
      Reply  : League.JSON.Objects.JSON_Object;
      Action : League.Strings.Universal_String :=
        Request.Header.Value (+"msg_type").To_String;
   begin
      if Action.Ends_With ("_request") then
         Action := Action.Head_To (Action.Length - 8);
         Topic := Action & "_reply";
      else
         --  No _request in msg_type, ignore it
         return;
      end if;

      declare
         Content : League.JSON.Objects.JSON_Object;
      begin
         Content.Insert (+"execution_state", -"busy");
         Send_Message
           (Frontend.IOPub,
            -Topic,
            Frontend.Key,
            "status",
            Parent  => Request.Header,
            Content => Content);
      end;

      if Action = +"kernel_info" then
         Kernel.Kernel_Info (Reply);
         Send_Message
           (Frontend.Shell,
            Request.From,
            Frontend.Key,
            "kernel_info_reply",
            Parent  => Request.Header,
            Content => Reply);
      elsif Action = +"execute" then
         declare
            S : constant League.Strings.Universal_String :=
              Request.Header.Value (+"session").To_String;
            Input  : League.JSON.Objects.JSON_Object := Request.Content;
            Values : League.JSON.Objects.JSON_Object;
            Error  : Jupyter.Kernels.Execution_Error;
            Object : Jupyter.Kernels.Session_Access;
            Count  : League.Holders.Universal_Integer;
            IO_Pub : aliased IO_Pubs.IO_Pub :=
              (Frontend'Unchecked_Access,
               Request.Header,
               others => <>);
         begin
            if not Map.Contains (S) then
               IO_Pub.Id := Next_Id;
               IO_Pub.Count := 1;
               Count := 1;
               Kernel.Create_Session (IO_Pub.Id, Object);
               Map.Insert (S, (IO_Pub.Id, IO_Pub.Count));
               Next_Id := Next_Id + 1;
            else
               Map (S).Count := Map (S).Count + 1;
               IO_Pub.Id := Map (S).Id;
               IO_Pub.Count := Map (S).Count;
               Object := Kernel.Get_Session (IO_Pub.Id);
               Count := League.Holders.Universal_Integer (IO_Pub.Count);
            end if;

            Input.Insert
              (+"execution_count",
               League.JSON.Values.To_JSON_Value (Count));

            Send_Message
              (Frontend.IOPub,
               -(+"execute_input"),
               Frontend.Key,
               "execute_input",
               Parent  => Request.Header,
               Content => Input);

            Object.Execute
              (IO_Pub            => IO_Pub'Unchecked_Access,
               Execution_Counter => IO_Pub.Count,
               Code              => Input.Value (+"code").To_String,
               Silent            => Input.Value (+"silent").To_Boolean,
               User_Expressions => Input.Value (+"user_expressions").To_Object,
               Allow_Stdin       => Input.Value (+"allow_stdin").To_Boolean,
               Stop_On_Error     => Input.Value (+"stop_on_error").To_Boolean,
               Expression_Values => Values,
               Error             => Error);

            Reply.Insert
              (+"execution_count",
               League.JSON.Values.To_JSON_Value (Count));

            if Error.Name.Is_Empty then
               Reply.Insert (+"status", -"ok");
               Reply.Insert (+"user_expressions", Values.To_JSON_Value);
            else
               Reply.Insert (+"status", -"error");
               Reply.Insert
                 (+"ename", League.JSON.Values.To_JSON_Value (Error.Name));
               Reply.Insert
                 (+"evalue", League.JSON.Values.To_JSON_Value (Error.Value));
               --  FIXME: Copy traceback
            end if;

            Send_Message
              (Frontend.Shell,
               Request.From,
               Frontend.Key,
               "execute_reply",
               Parent  => Request.Header,
               Content => Reply);
         end;
      end if;

      declare
         Content : League.JSON.Objects.JSON_Object;
      begin
         Content.Insert (+"execution_state", -"idle");
         Send_Message
           (Frontend.IOPub,
            -Topic,
            Frontend.Key,
            "status",
            Parent  => Request.Header,
            Content => Content);
      end;
   end Process_Shell_Message;

   --------------------------
   -- Read_Connection_File --
   --------------------------

   procedure Read_Connection_File
     (Name   : League.Strings.Universal_String;
      Result : out League.JSON.Objects.JSON_Object)
   is
      Input : Ada.Wide_Wide_Text_IO.File_Type;
      Text  : League.Strings.Universal_String;
      Doc   : League.JSON.Documents.JSON_Document;
   begin
      Ada.Wide_Wide_Text_IO.Open
        (Input, Ada.Wide_Wide_Text_IO.In_File, Name.To_UTF_8_String);
      while not Ada.Wide_Wide_Text_IO.End_Of_File (Input) loop
         declare
            Line : constant Wide_Wide_String :=
              Ada.Wide_Wide_Text_IO.Get_Line (Input);
         begin
            Text.Append (Line);
            Text.Append (Ada.Characters.Wide_Wide_Latin_1.LF);
         end;
      end loop;
      Ada.Wide_Wide_Text_IO.Close (Input);
      Doc := League.JSON.Documents.From_JSON (Text);
      Result := Doc.To_JSON_Object;
   end Read_Connection_File;

   ------------------
   -- Read_Message --
   ------------------

   procedure Read_Message
     (Socket : ZMQ.Sockets.Socket;
      Key    : League.Strings.Universal_String;
      Result : out Message)
   is
      procedure Read_Object
        (Object : out League.JSON.Objects.JSON_Object;
         More   : out Boolean);

      Digest : GNAT.SHA256.Context :=
        GNAT.SHA256.HMAC_Initial_Context (Key.To_UTF_8_String);

      -----------------
      -- Read_Object --
      -----------------

      procedure Read_Object
        (Object : out League.JSON.Objects.JSON_Object;
         More   : out Boolean)
      is
         MSG : ZMQ.Messages.Message;
      begin
         MSG.Initialize (0);
         Socket.Recv (MSG);
         declare
            Text : constant String := MSG.GetData;
         begin
            More := Has_More (MSG);
            GNAT.SHA256.Update (Digest, Text);
            Object := League.JSON.Documents.From_JSON
              (League.Strings.From_UTF_8_String (Text)).To_JSON_Object;
         end;
      end Read_Object;

      More : Boolean;
   begin
      loop
         declare
            From : ZMQ.Messages.Message;
            Item : League.Stream_Element_Vectors.Stream_Element_Vector;
         begin
            From.Initialize (0);
            Socket.Recv (From);
            exit when From.GetData = "<IDS|MSG>";
            Item := League.Stream_Element_Vectors.To_Stream_Element_Vector
              (From.GetData);
            Result.From.Append (Item);
            pragma Assert (Has_More (From));
         end;
      end loop;

      Result.Signature := League.Strings.From_UTF_8_String (Socket.Recv);
      Read_Object (Result.Header, More);
      pragma Assert (More);
      Read_Object (Result.Parent, More);
      pragma Assert (More);
      Read_Object (Result.Metadata, More);
      pragma Assert (More);
      Read_Object (Result.Content, More);

      while More loop  --  Skip buffers if any
         declare
            Temp : ZMQ.Messages.Message;
         begin
            Temp.Initialize (0);
            Socket.Recv (Temp);
            More := Has_More (Temp);
         end;
      end loop;

   end Read_Message;

   -----------------
   -- Send_Status --
   -----------------

   procedure Send_Message
     (Socket  : in out ZMQ.Sockets.Socket;
      To      : Address_Lists.List;
      Key     : League.Strings.Universal_String;
      Kind    : Wide_Wide_String;
      Parent  : League.JSON.Objects.JSON_Object :=
        League.JSON.Objects.Empty_JSON_Object;
      Content : League.JSON.Objects.JSON_Object :=
        League.JSON.Objects.Empty_JSON_Object)
   is
      use type League.Strings.Universal_String;
      Object : League.JSON.Objects.JSON_Object;
      Digest : GNAT.SHA256.Context :=
        GNAT.SHA256.HMAC_Initial_Context (Key.To_UTF_8_String);
   begin
      Object.Insert
        (+"msg_id",
         League.JSON.Values.To_JSON_Value
           (Parent.Value (+"msg_id").To_String & Kind));

      Object.Insert (+"session", Parent.Value (+"session"));
      Object.Insert (+"username", Parent.Value (+"username"));
      if Parent.Contains (+"date") then
         Object.Insert (+"date", Parent.Value (+"date"));
      end if;
      Object.Insert (+"msg_type", -Kind);
      Object.Insert (+"version", -"5.3");

      GNAT.SHA256.Update
        (Digest,
         Object.To_JSON_Document.To_JSON.To_Stream_Element_Array);

      GNAT.SHA256.Update
        (Digest,
         Parent.To_JSON_Document.To_JSON.To_Stream_Element_Array);

      GNAT.SHA256.Update (Digest, "{}");

      GNAT.SHA256.Update
        (Digest,
         Content.To_JSON_Document.To_JSON.To_Stream_Element_Array);

      for X of To loop
         Socket.Send (X.To_Stream_Element_Array, 2);
      end loop;

      Socket.Send ("<IDS|MSG>", 2);
      Socket.Send (String'(GNAT.SHA256.Digest (Digest)), 2);
      Socket.Send
        (Object.To_JSON_Document.To_JSON.To_Stream_Element_Array,
         2);
      Socket.Send
        (Parent.To_JSON_Document.To_JSON.To_Stream_Element_Array,
         2);
      Socket.Send ("{}", 2);
      Socket.Send (Content.To_JSON_Document.To_JSON.To_Stream_Element_Array);
   end Send_Message;

   use type Interfaces.C.int;
   use type Interfaces.C.long;
   use type Interfaces.C.short;

   CF : League.JSON.Objects.JSON_Object;

   Frontend : aliased Frontend_Connection;
   Poll     : array (1 .. 4) of aliased ZMQ.Low_Level.zmq_pollitem_t;
begin
   Read_Connection_File (File, CF);

   Frontend.Key := CF.Value (+"key").To_String;

   Bind (Frontend.Ctx, Frontend.Shell, CF, "shell_port", ZMQ.Sockets.ROUTER);
   Bind (Frontend.Ctx, Frontend.Stdin, CF, "stdin_port", ZMQ.Sockets.ROUTER);
   Bind (Frontend.Ctx, Frontend.IOPub, CF, "iopub_port", ZMQ.Sockets.PUB);
   Bind (Frontend.Ctx, Frontend.Ping, CF, "hb_port", ZMQ.Sockets.ROUTER);
   Bind
     (Frontend.Ctx,
      Frontend.Control,
      CF,
      "control_port",
      ZMQ.Sockets.ROUTER);

   loop
      Poll :=
        (1 => (socket => Frontend.Shell.Get_Impl,
               fd     => 0,
               events => ZMQ.Low_Level.Defs.ZMQ_POLLIN,
               revents => 0),
         2 => (socket  => Frontend.Stdin.Get_Impl,
               fd      => 0,
               events  => ZMQ.Low_Level.Defs.ZMQ_POLLIN,
               revents => 0),
         3 => (socket  => Frontend.Ping.Get_Impl,
               fd      => 0,
               events  => ZMQ.Low_Level.Defs.ZMQ_POLLIN,
               revents => 0),
         4 => (socket => Frontend.Control.Get_Impl,
               fd     => 0,
               events => ZMQ.Low_Level.Defs.ZMQ_POLLIN,
               revents => 0));

      if ZMQ.Low_Level.zmq_poll
        (items_u   => Poll (1)'Access,
         nitems_u  => 4,
         timeout_u => -1) < 0
      then
         return;
      end if;

      if Poll (1).revents /= 0 then
         declare
            Msg : Message;
         begin
            Read_Message (Frontend.Shell, Frontend.Key, Msg);
            Process_Shell_Message (Frontend, Msg);
         end;
      end if;
   end loop;
end Jupyter.Start_Kernel;
