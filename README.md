# Caveat

This project has been divided into the following projects:
* [Diff/AST](https://github.com/codinuum/diffast) and
* [CCA-Base](https://github.com/codinuum/cca-base).

Hence it will be archived soon.

# Code Continuity Analysis Framework

The framework is currently composed of the following:

* parsers for Python, Java, Verilog, Fortran, and C/C++,
* an AST differencing tool, Diff/AST, based on the parsers,
* helper scripts for factbase manipulation, and
* ontologies for the related entities.

The parsers and Diff/AST export resulting *facts* such as abstract syntax trees (ASTs), changes between them, and other syntactic/semantic information in
[XML](https://www.w3.org/TR/xml11/) or [N-Triples](https://www.w3.org/2001/sw/RDFCore/ntriples/).
In particular, facts in N-Triples format are loaded into an RDF store such as
[Virtuoso](https://github.com/openlink/virtuoso-opensource) to build a *factbase* or a database of facts.
Factbases are intended to be queried for software engineering tasks such as
[code comprehension](https://github.com/ebt-hpc/cca),
[debugging](https://stair.center/archives/research/ddj-esecfse2018),
[change pattern mining](https://ieeexplore.ieee.org/document/7081845), and
[code homology analysis](https://link.springer.com/chapter/10.1007/978-3-642-12029-9_7).

Diff/AST is an experimental implementation of the AST differencing algorithm
reported in the following paper:

Masatomo Hashimoto and Akira Mori, "Diff/TS: A Tool for Fine-Grained Structural Change Analysis,"
In *Proc. 15th Working Conference on Reverse Engineering*, 2008, pp. 279-288,
DOI: [10.1109/WCRE.2008.44](https://doi.org/10.1109/WCRE.2008.44).

It compares ASTs node by node, while popular `diff` tools compare any (text) files line by line.
The algorithm is based on [an algorithm](https://doi.org/10.1137/0218082) for computing *tree edit distance (TED)* between two ordered labeled trees.  The TED between two trees is the minimum (weighted) number of edit operations to transform one tree into another.
Unfortunately, applying TED algorithms directly to wild ASTs is not feasible in general because [their computational complexity is essentially, at best, quadratic according to the number of AST nodes](https://doi.org/10.1016/j.tcs.2004.12.030).
Therefore Diff/TS makes moderate use of a TED algorithm in a divide-and-conquer manner backed by elaborated heuristics to approximate tree edit distances.
Nevertheless, Diff/AST still requires much time for non-trivial massive inputs. Thus it always caches the results.

## Screenshots

You can see the results of comparing some pairs of source files taken from [samples](samples) [here](https://codinuum.github.io/gallery-cca).

## Quick start

You can instantly try Diff/AST by utilizing [Docker](https://www.docker.com/) and [a ready-made container image](https://hub.docker.com/r/codinuum/cca).

    $ docker pull codinuum/cca

The following command line executes Diff/AST within a container to compare sample Java programs and then saves the results in `results` (host) directory.

    $ ./cca.py diffast -c results samples/java/0/Test.java samples/java/1/Test.java

Once you have built [DiffViewer](diffviewer), you can inspect the AST differences in a viewer window. See [`diffviewer/README.md`](diffviewer/README.md) for details.

    $ diffviewer/run.py -c results samples/java/0/Test.java samples/java/1/Test.java

You can run both Diff/AST and DiffViewer by the following line.

    $ ./cca.py diffast -c results --view samples/java/0/Test.java samples/java/1/Test.java

## Installing parsers and Diff/AST

### Requirements

* [OCaml](http://ocaml.org/) (>=4.14.0 and < 5.0.0)
* [OPAM](https://opam.ocaml.org/)

### Installation

The following will install `parsesrc` and `diffast`.

    $ opam install cca

## Building parsers and Diff/AST

You can also build parsers and Diff/AST in person.

### Requirements

* GNU make
* [OCaml](http://ocaml.org/) (>=4.11.1)
* [OPAM](https://opam.ocaml.org/) (for installing camlzip, cryptokit, csv, git-unix, menhir, ocamlnet, pxp, ulex, uuidm, and volt.)

### Compilation

The following create `ast/analyzing/bin/{parsesrc.opt,diffast.opt}`.

    $ cd src
    $ make

They should be used via shell scripts `ast/analyzing/bin/{parsesrc,diffast}` to set some environment variables.

## Using with Git

If you have built Diff/AST, you can use it with Git. Add the following lines to your `.gitconfig`. Note that `PATH_TO_THIS_REPO` should be replaced by your local path to this repository.

    [diff]
        tool = diffast
    [difftool]
        prompt = false
    [difftool "diffast"]
        cmd = PATH_TO_THIS_REPO/git_ext_diff "$LOCAL" "$REMOTE"
    [alias]
        diffast = difftool

Then you should be able to use `git diffast` like `git diff`. You will be prompted to launch diffast for each source file comparison. Other file comparisons will be ignored.


## Building docker image

The following command line creates a docker image named `cca`.  In the image, the framework is installed at `/opt/cca`.

    $ docker build -t cca .

## License

Apache License, Version 2.0
