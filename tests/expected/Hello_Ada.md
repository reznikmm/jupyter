# Jupiter Ada kernel

The kernel accepts an Ada program piece by piece. A piece could be:
1. A sequence of [context clauses](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-10-1-2.html#S0253):


```Ada
with Ada.Text_IO;
```

2. A sequence of [statements](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-5-1.html#S0146) to be executed:


```Ada
Ada.Text_IO.Put_Line ("Hello World");
```




    Hello World




3. A sequence of [basic declarative items](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-3-11.html#S0088) to be elaborated:


```Ada
X : Integer := 1;
```

There are also some "magic" commands:


```Ada
%lsmagic
```




    Available line magics:
    %lsmagic


