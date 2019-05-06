# Application template

Starting point for an application project. It is not a framework but a source template to be
adjusted to the requirements at hand.
It features:

- install signal handlers for process termination or user defined signal implementation
- termination signals are transformed to asynchronous exception to be handled by user
  threads. An example of masking this exception for an unterruptable operation is given
- log to file or console with different log levels
- start asynchronous event handlers like webservers or timed polling functions

## Generate html docs

- generate docs with `haddock.sh` (see output for path to index.html)
- generate `README.html` with `readme.sh` (needs the `markdown` tool to be installed)

## Generate module dependency graphs

- generate `src.png` and `test.png` with `dep.sh` (needs the `graphmod` tool)

## Generate code coverage

- run `coverage.sh`

## Module dependency graphs

- Source dependencies ![](src.png)
- Test code dependencies (prunded edges) ![](test.png)
