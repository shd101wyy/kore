// Copyright (c) 2019 K Team. All Rights Reserved.

module LABELED-CLAIMS-SYNTAX
    imports INT
    syntax KItem ::= Pgm

    syntax Pgm ::= "a" | "b" | "c"
endmodule

module LABELED-CLAIMS
    imports LABELED-CLAIMS-SYNTAX

    configuration
      <k> $PGM:Pgm </k>

    rule <k> a => b ... </k>
    rule <k> b => c ... </k>
endmodule
