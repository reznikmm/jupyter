--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with League.Strings;
with League.JSON.Arrays;
with League.JSON.Objects;
with League.JSON.Values;
with League.String_Vectors;

package Jupyter.Kernels is

   Version : constant Wide_Wide_String := "5.3";
   --  The current version of the specification is 5.3.

   type Kernel is limited interface;
   --  Global kernel request interface

   type Session is limited interface;
   --  Per session request interface

   type Session_Access is access all Session'Class with Storage_Size => 0;

   type IO_Pub is limited interface;
   --  this socket is the ‘broadcast channel’ where the kernel publishes
   --  all side effects (stdout, stderr, debugging events etc.) as well as
   --  the requests coming from any client over the shell socket and its own
   --  requests on the stdin socket.

   type IO_Pub_Access is access all IO_Pub'Class with Storage_Size => 0;

   not overriding procedure Kernel_Info
     (Self   : aliased in out Kernel;
      Result : out League.JSON.Objects.JSON_Object) is abstract;
   --  If a client needs to know information about the kernel, it can make
   --  a request of the kernel’s information. This message can be used to
   --  fetch core information of the kernel, including language (e.g., Python),
   --  language version number and IPython version number, and the IPython
   --  message spec version number.

   not overriding procedure Shutdown (Self : aliased in out Kernel) is null;
   --  The client sends a shutdown request to the kernel, and once it receives
   --  the reply message (which is otherwise empty), it can assume that the
   --  kernel has completed shutdown safely.

   not overriding procedure Create_Session
     (Self       : aliased in out Kernel;
      Session_Id : Positive;
      Result     : out Session_Access) is abstract;
   --  Create a new session with unique Id

   not overriding procedure Destroy_Session
     (Self       : aliased in out Kernel;
      Session_Id : Positive) is null;
   --  Deallocate the sesstion with given Id

   not overriding procedure Interrupt
     (Self       : aliased in out Kernel;
      Session_Id : Positive) is null;
   --  Kernel interrupt

   not overriding procedure Debug
     (Self       : aliased in out Kernel;
      Session_Id : Positive;
      Content    : out League.JSON.Objects.JSON_Object) is null;
   --  This message type is used with debugging kernels to request specific
   --  actions to be performed by the debugger such as adding a breakpoint
   --  or stepping into a code.
   --
   --  @param Content  The content dict can be any JSON information used by
   --  debugging frontends and kernels.

   not overriding function Get_Session
     (Self       : aliased in out Kernel;
      Session_Id : Positive) return Session_Access is abstract;
   --  Get a session by Id

   not overriding procedure Save_History
     (Self       : aliased in out Kernel;
      Session_Id : Positive;
      Input      : League.JSON.Values.JSON_Value;
      Output     : League.JSON.Values.JSON_Value) is null;
   --  Save history element for a session with given Id

   type History_Access_Type is (A_Range, A_Tail, A_Search);
   --  Kind of history reply. So far, this can be 'range', 'tail' or 'search'.

   type History_Access (Kind : History_Access_Type := A_Range) is record
      Session : Positive;
      --  Session is a number counting up each time the kernel starts;
      --  you can give a positive session number, or a negative number
      --  to count back from the current session.

      case Kind is
         when A_Range =>
            --  Get a range of input cells.

            Start, Stop : Positive;
            --  start and stop are line (cell) numbers within that session.

         when A_Tail | A_Search =>
            Count : Positive;  --  Get the last Count cells.

            case Kind is
               when A_Search =>
                  Pattern : League.Strings.Universal_String;
                  --  Get cells matching the specified glob pattern (with * and
                  --  ? as wildcards).

                  Unique  : Boolean;
                  --  Do not include duplicated history.
               when others =>
                  null;
            end case;
      end case;
   end record;

   not overriding procedure History
     (Self           : aliased in out Kernel;
      Output         : Boolean;
      Raw            : Boolean;
      History_Access : Jupyter.Kernels.History_Access;
      History        : out League.JSON.Arrays.JSON_Array) is null;
   --  For clients to explicitly request history from a kernel. The kernel has
   --  all the actual execution history stored in a single location, so clients
   --  can request it from the kernel when needed.
   --
   --  @param Output  If True, also return output history in the resulting
   --  dict.
   --
   --  @param Raw  If True, return the raw input history, else the transformed
   --  input.
   --
   --  @param History  A list of 3 tuples, either:
   --  * (session, line_number, input) or
   --  * (session, line_number, (input, output)),
   --  depending on whether Output was False or True, respectively.

   type Execution_Error is record
      Name      : League.Strings.Universal_String;
      Value     : League.Strings.Universal_String;
      Traceback : League.String_Vectors.Universal_String_Vector;
   end record;

   function No_Execution_Error return Execution_Error is ((others => <>));

   not overriding procedure Execute
     (Self              : aliased in out Session;
      IO_Pub            : not null IO_Pub_Access;
      Execution_Counter : Positive;
      Code              : League.Strings.Universal_String;
      Silent            : Boolean;
      User_Expressions  : League.JSON.Objects.JSON_Object;
      Allow_Stdin       : Boolean;
      Stop_On_Error     : Boolean;
      Expression_Values : out League.JSON.Objects.JSON_Object;
      Error             : in out Execution_Error) is abstract;
   --  Execute code on behalf of the user, in a namespace reserved to the
   --  user’s variables (and thus separate from the kernel’s own internal
   --  code and variables).
   --
   --  @param Execution_Counter  The global kernel counter that increases by
   --  one with each request that stores history. This will typically be used
   --  by clients to display prompt numbers to the user. If the request did not
   --  store history, this will be the current value of the counter in the
   --  kernel.
   --
   --  @param Code  Source code to be executed by the kernel, one or more
   --  lines.
   --
   --  @param Silent  A boolean flag which, if True, signals the kernel
   --  to execute this code as quietly as possible. silent=True forces
   --  store_history to be False, and will *not*:
   --  * broadcast output on the IOPUB channel
   --  * have an execute_result
   --
   --  @param User_Expressions  A dict mapping names to expressions to be
   --  evaluated in the user's dict. The rich display- data representation
   --  of each will be evaluated after execution.
   --
   --  @param Allow_Stdin Some frontends do not support stdin requests.
   --  If this is true, code running in the kernel can prompt the user for
   --  input with an input_request message (see below). If it is false, the
   --  kernel should not send these messages.
   --
   --  @param Stop_On_Error If True, aborts the execution queue if an exception
   --  is encountered. If False, queued execute_requests will execute even if
   --  this request generates an exception.
   --
   --  @param Expression_Values  Results for the User_Expressions.

   not overriding procedure Complete
     (Self         : aliased in out Session;
      Code         : League.Strings.Universal_String;
      Cursor_Pos   : Positive;
      Matches      : out League.JSON.Arrays.JSON_Array;
      Cursor_Start : out Positive;
      Cursor_End   : out Positive;
      Metadata     : out League.JSON.Objects.JSON_Object) is null;
   --  Completion request
   --
   --  @param Code  The code context in which completion is requested this may
   --  be up to an entire multiline cell, such as 'foo = a.isal'
   --
   --  @param Cursor_Pos  The cursor position within 'Code' (in unicode
   --  characters) where completion is requested
   --
   --  @param Matches  The list of all matches to the completion request, such
   --  as ['a.isalnum', 'a.isalpha'] for the above example.
   --
   --  @param Cursor_Start  The range of text that should be replaced by the
   --  above matches when a completion is accepted. typically cursor_end is the
   --  same as cursor_pos in the request.
   --
   --  @param Cursor_End  See Cursor_Start
   --
   --  @param Metadata  Information that frontend plugins might use for extra
   --  display information about completions.

   not overriding procedure Inspect
     (Self         : aliased in out Session;
      Code         : League.Strings.Universal_String;
      Cursor_Pos   : Positive;
      Detail_Level : Natural;
      Found        : out Boolean;
      Data         : out League.JSON.Objects.JSON_Object;
      Metadata     : out League.JSON.Objects.JSON_Object) is null;
   --  Code can be inspected to show useful information to the user. It is up
   --  to the Kernel to decide what information should be displayed, and its
   --  formatting.
   --
   --  @param Code  The code context in which introspection is requested this
   --  may be up to an entire multiline cell.
   --
   --  @param Cursor_Pos  The cursor position within 'Code' (in unicode
   --  characters) where inspection is requested.
   --
   --  @param Detail_Level  The level of detail desired. In IPython, the
   --  default (0) is equivalent to typing 'x?' at the prompt, 1 is equivalent
   --  to 'x??'. The difference is up to kernels, but in IPython level 1
   --  includes the source code if available.
   --
   --  @param Found  Found should be true if an object was found
   --
   --  @param Data  Data can be empty if nothing is found

   type Completeness_Status is (Complete, Incomplete, Invalid, Unknown);
   --  Kind of completeness reply
   --
   --  @value Complete    Code is ready to be executed
   --  @value Incomplete  Code should prompt for another line
   --  @value Invalid     Code will typically be sent for execution, so that
   --  the user sees the error soonest
   --
   --  @value Unknown if the kernel is not able to determine this. The frontend
   --  should also handle the kernel not replying promptly. It may default
   --  to sending the code for execution, or it may implement simple fallback
   --  heuristics for whether to execute the code (e.g. execute after a blank
   --  line).

   not overriding procedure Is_Complete
     (Self   : aliased in out Session;
      Code   : League.Strings.Universal_String;
      Status : in out Completeness_Status;
      Indent : out League.Strings.Universal_String) is null;
   --  When the user enters a line in a console style interface, the console
   --  must decide whether to immediately execute the current code, or whether
   --  to show a continuation prompt for further input.
   --
   --  @param Code    The code entered so far as a multiline string
   --  @param Status  One of 'complete', 'incomplete', 'invalid', 'unknown'
   --  @param Indent  If status is 'incomplete', indent should contain the
   --  characters to use to indent the next line. This is only a hint:
   --  frontends may ignore it and use their own autoindentation rules.
   --  For other statuses, this field does not exist.

   ------------
   -- IO_Pub --
   ------------

   not overriding procedure Stream
     (Self : in out IO_Pub;
      Name : League.Strings.Universal_String;
      Text : League.Strings.Universal_String) is abstract;
   --  Streaming output
   --
   --  @param Name  The name of the stream is one of 'stdout', 'stderr'
   --  @param Text  The text is an arbitrary string to be written to that
   --  stream

   not overriding procedure Display_Data
     (Self      : in out IO_Pub;
      Data      : League.JSON.Objects.JSON_Object;
      Metadata  : League.JSON.Objects.JSON_Object;
      Transient : League.JSON.Objects.JSON_Object) is abstract;
   --  This type of message is used to bring back data that should be displayed
   --  (text, html, svg, etc.) in the frontends. This data is published to all
   --  frontends. Each message can have multiple representations of the data;
   --  it is up to the frontend to decide which to use and how. A single
   --  message should contain all possible representations of the same
   --  information. Each representation should be a JSON’able data
   --  structure, and should be a valid MIME type.
   --
   --  @param Data  The data dict contains key/value pairs, where the keys are
   --  MIME types and the values are the raw data of the representation in that
   --  format
   --  @param Metadata  Any metadata that describes the data
   --  @param Transient  Optional transient data introduced in 5.1. Information
   --  not to be persisted to a notebook or other documents. Intended to live
   --  only during a live kernel session.

   not overriding procedure Update_Display_Data
     (Self      : in out IO_Pub;
      Data      : League.JSON.Objects.JSON_Object;
      Metadata  : League.JSON.Objects.JSON_Object;
      Transient : League.JSON.Objects.JSON_Object) is abstract;

   not overriding procedure Execute_Result
     (Self      : in out IO_Pub;
      Data      : League.JSON.Objects.JSON_Object;
      Metadata  : League.JSON.Objects.JSON_Object;
      Transient : League.JSON.Objects.JSON_Object) is abstract;
   --  Results of an execution are published as an execute_result. These
   --  are identical to display_data messages, with the addition of an
   --  execution_count key.

   not overriding procedure Execute_Error
     (Self  : in out IO_Pub;
      Value : Execution_Error) is abstract;
   --  When an error occurs during code execution

   not overriding procedure Clear_Output
     (Self : in out IO_Pub;
      Wait : Boolean) is abstract;
   --  This message type is used to clear the output that is visible on the
   --  frontend.
   --
   --  @param Wait  Wait to clear the output until new output is available.
   --  Clears the existing output immediately before the new output is
   --  displayed. Useful for creating simple animations with minimal
   --  flickering.

   not overriding procedure Debug_Event
     (Self    : in out IO_Pub;
      Content : League.JSON.Objects.JSON_Object) is abstract;
   --  This message type is used by debugging kernels to send debugging events
   --  to the frontend.
   --
   --  @param Content  The content dict can be any JSON information used by
   --  debugging frontends.

end Jupyter.Kernels;
