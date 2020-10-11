# Jupiter Ada kernel

The kernel accepts an Ada program piece by piece. A piece could be:
1. A sequence of [context clauses](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-10-1-2.html#S0253):


```Ada
with Ada.Text_IO;
```

2. A sequence of [declarative items](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-3-11.html#S0087) to be elaborated:


```Ada
Name : constant String := "World";
```

3. A sequence of [statements](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-5-1.html#S0146) to be executed:


```Ada
Ada.Text_IO.Put_Line ("Hello " & Name);
```




    Hello World




Note: If one of declarative item requires a completion, then it should be in the same cell:


```Ada
package P is
   procedure Proc;
end;

package body P is
   procedure Proc is
   begin
      Ada.Text_IO.Put_Line ("Here is Proc!");
   end;
end;
```

There are also some "magic" commands:


```Ada
%lsmagic
```




    Available line magics:
    %lsmagic? %%output? %%writefile? %gargs? %cargs? %largs? %bargs?


