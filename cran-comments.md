---
title: CRAN package `readmoRe`
---

## Re-submission 2025-02-28
This is a re-submission. The version was increased to 0.2-14 after addressing the
issues arising in pre-tests:

* moved directory `extdata` to `inst/extdata`

Additionally, an issue causing two examples to fail was addressed.
The problem originated in a change in the example data collection included in the
'readxl' package to v.1.4.4 which removed the iris dataset.
In addition, a formatting error in one documentation file was addressed and the package
overview documentation was updated to now using the special "_PACKAGE" sentinel.

### Test environments (2025-02-28 - )
* local OS X install: x86_64-apple-darwin24.2.0, R 4.4.2
* win-builder (devel, release and oldrelease)
* Red Hat Enterprise Linux release 9.5 (Plow), R 4.4.2

## Test environments
* local OS X install: x86_64-apple-darwin17.0, R 4.0.2
* win-builder (devel and release)
* CentOS Linux release 7.9.2009 (Core) [:core-4.1-amd64:core-4.1-noarch], R 4.0.4

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

```
R CMD check --as-cran readmoRe_0.2-12.tar.gz
.
* checking CRAN incoming feasibility ... NOTE     
Maintainer: ‘Vidal Fey <vidal.fey@gmail.com>’
.
```
