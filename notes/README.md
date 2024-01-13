# CraZy core 2 - an atomspacy toolkit
A toolkit for prototyping knowledge bases, programming languages, algorithms for machine learning, and everything in between.  

In contrast to the [Atomspace](https://github.com/opencog/atomspace/) which uses postfix compression (sharing subtrees), this repository has tools for a prefix compression knowledge base (sharing roots). 
This results in performance in opposite areas, for example, here you have fast lookup given a pattern but it's slow to find all occurrences of a variable or subexpression.
See [SPJ's paper](https://simon.peytonjones.org/assets/pdfs/triemaps-that-match.pdf) for the inspiration of this repo and [Linas his paper](https://github.com/opencog/atomspace/blob/master/opencog/sheaf/docs/ram-cpu.pdf) for an overview of the Atomspace implementation's rationale.

The purpose of this toolkit is to provide strong yet modular components for implementing a kernel that can efficiently story, query, and process groups of S-expressions.
Extending the analogy, the kernel still requires an operating system (that does IO, provides an environment for users to organize their data, and makes decisions on the API), and finally applications (like a business process manager or Minecraft agent), to be useful.

Importantly, the toolkit separates storage mechanisms and evaluation policies completely, and can be mixed an matched (potentially within the same kernel).
Practically, this means you can have a mismatch of [process calculus](https://en.wikipedia.org/wiki/Process_calculus), multivalued lambda calculus like in [MeTTa](https://wiki.opencog.org/w/File:MeTTa_Specification.pdf), and optimized [WASM](https://webassembly.org/) stack machine.

This toolkit compiles to native, JS, and the JVM.

For an overview of what this toolkit provides, see [the overview](overview.md).

If you have any questions, please [open an issue](https://github.com/Adam-Vandervorst/CZ2/issues/new).
