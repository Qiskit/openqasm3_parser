## Code generation for the crate oq3_syntax

### Do I need to run code generation?

If you don't modify the "ungrammar" [./crates/oq3_syntax/openqasm3.ungram](./crates/oq3_syntax/openqasm3.ungram)
then you don't need to generate any code to build or develop openqasm_parser, and you can stop reading here.

If you *do* modify [./crates/oq3_syntax/openqasm3.ungram](./crates/oq3_syntax/openqasm3.ungram), then read on.

NOTE: These (rather simple) scripts controlling codegen have been developed under fedora linux. They may need
to be modified for your OS. The codegen itself is done in rust and should run correctly on most
platforms, or should be easily made to do so.

The source files in [../crates/oq3_syntax/src/ast/generated](../crates/oq3_syntax/src/ast/generated) are generated via scripts
that are run during development.
They do not need to be run when building the crates if the input files to the codegen have not been modified.

On the other hand, if the input files to codegen have been modified, then codegen scripts
must be run by calling [./mkgenerated](./mkgenerated). See comments in [./mkgenerated](./mkgenerated).

Run ./mkgenerated from this directory to write generated rust source to temporary files.
Trying to run ./mkgenerated from the root of this repo will fail to generate the required code.
Run [./cpnodes.sh](./cpnodes.sh) and [./cpgenerated.sh](./cpgenerated.sh) to overwrite the revision-controlled source
files with the temporary files.

> [!IMPORTANT]
> [./mkgenerated](./mkgenerated) runs [./mk_nodes.sh](./mk_nodes.sh) twice. The first time errors are reported such as: `test tests::sourcegen_ast::write_syntax_kinds_enum ... FAILED`. These errors can be ignored. The second time [./mk_nodes.sh](./mk_nodes.sh) runs no errors should be reported.
