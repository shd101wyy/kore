module SET-ACCESS
    imports MAP
    imports SET
    imports BOOL

    configuration <imp>
                    <k> $PGM:Stmt </k>
                    <mem> .Map </mem>
                  </imp>

    syntax Key ::= "s" | "n"

    syntax Stmt ::= "init" | "abstract"
 // -----------------------------------
    rule <k> init => abstract ... </k>
         <mem> .Map => n |-> 10 s |-> 10 </mem>

    rule <k> abstract => #abstract(keys(MEM)) ... </k>
         <mem> MEM </mem>

    syntax KItem ::= #abstract ( Set )
 // ----------------------------------
    rule <k> #abstract(.Set) => . ... </k>

    rule <k> #abstract((SetItem(X) => .Set) XS) ... </k>
         <mem> MEM => MEM[X <- 0] </mem>
endmodule
