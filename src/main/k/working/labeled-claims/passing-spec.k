// Copyright (c) 2019 K Team. All Rights Reserved.

module VERIFICATION
    imports LABELED-CLAIMS
endmodule

module PASSING-SPEC
    imports VERIFICATION
    rule [aname] : <k> a => c ... </k>
endmodule
