# Code Comprehension Assistance Framework

The framework is currently composed of the following:

* parsers for Python, Java, and Verilog,
* helper scripts for factbase manipulation, and
* ontologies for related entities.

The parsers export resulting *facts* such as ASTs and
other syntactic information in [XML](https://www.w3.org/TR/xml11/) or
[N-Triples](https://www.w3.org/2001/sw/RDFCore/ntriples/).
In particular, facts in N-Triples format are loaded into an RDF store such as
[Virtuoso](https://github.com/openlink/virtuoso-opensource) to build a
*factbase*, or a database of facts.
Factbases are intended to be queried for code comprehension tasks.

## Requirements

* GNU make
* [OCaml](http://ocaml.org/) (>=4.02)
* [Findlib](http://projects.camlcity.org/projects/findlib.html)
* [Menhir](http://gallium.inria.fr/~fpottier/menhir/)
* [Ocamlnet](http://projects.camlcity.org/projects/ocamlnet.html) (>=4.1.0)
* [PXP](http://projects.camlcity.org/projects/pxp.html) (>=1.2.8)
* [Cryptokit](https://github.com/xavierleroy/cryptokit)
* [Camlzip](https://github.com/xavierleroy/camlzip)
* [OCaml CSV](https://github.com/Chris00/ocaml-csv)
* [Uuidm](http://erratique.ch/software/uuidm)
* [Volt](https://github.com/codinuum/volt)

## Compilation

The following create `ast/analyzing/bin/parsesrc.opt`.

    $ cd src
    $ make

It is called from a shell script `ast/analyzing/bin/parsesrc`.

## License

Apache License, Version 2.0
