
# Installation

## Prerequisites
the `handin-server` package needs to be installed.
If it is not, you can fix this by pulling `git@github.com:ps-tuebingen/handin.git` and running 
```
raco pkg install -n handin $(pwd)
```
in it.

## Actual installation

Install this via
```
raco pkg install -n grading-statistics $(pwd)
```

# Usage

Example:
```
racket -l statistics-tool stats <handin-directory>
```
should give statistics about the number of grade files etc.

