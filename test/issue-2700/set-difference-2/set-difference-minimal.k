requires "domains.md"

module SET-DIFFERENCE-MINIMAL
    imports INT
    imports SET

    configuration <k> $PGM:Step </k>

    syntax Step ::= run ( Set ) | done ( Set )

    rule [step]: <k> run(S) => done(S) ... </k>

    rule (SetItem(X) -Set SetItem(Y)) => SetItem(X)
         requires X =/=Int Y

endmodule