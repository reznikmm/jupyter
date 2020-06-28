# Ada Jupiter kernel

The kernel accepts an Ada program piece by piece. A piece could be:
* A with and use clauses that go before a compilation unit:


```Hello World
with Ada.Text_IO;
```




    with Ada.Text_IO; is with-clause!



* A Sequence of statements to be executed:


```Hello World
Ada.Text_IO.Put_Line ("aaa");
```




    aaa





```Hello World
%lsmagic
```




    Available line magics:
    %lsmagic


