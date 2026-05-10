# Workshop Script Tests

There are no standalone workshop-script test files in this directory at
present. The package-level regression tests live in `tests/testthat/` at the
repository root.

Run the current test suite from the package root:

```r
devtools::test()
```

Or from a shell:

```sh
Rscript -e "devtools::test()"
```

The tests cover the simulation helpers, selected end-to-end package workflows,
and optional-stack smoke tests where the relevant packages are available.
