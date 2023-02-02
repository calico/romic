## Test environments
* local OS X install, R 4.1.2
* Ubuntu 20.04 - Devel (Github Actions), R 3.6.3
* Ubuntu 20.04 - Release (Github Actions), R 3.6.3
* Windows - latest (Github Actions), R 4.2.2
* OSX - latest (Github Actions), R 4.2.2

## R CMD check results
R CMD check --as-cran romic_1.1.1.tar.gz 

There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Sean Hackett <sean@calicolabs.com>’

New submission

CRAN repository db overrides:
  X-CRAN-Comment: Removed on 2021-05-18 for policy violation.

  Detritus in /tmp

To address this issue I wrapped all export functions' examples in interactive()
checks. Since these examples would no longer run regularly, I instead added
tests of each export function. To ensure this wouldn't cause similar problem
to the above, I deleted all objects added to /tmp and confirmed removal.
Apologies to the CRAN team for this oversight. At Professor Brian Ripley's
request I delayed resubmission of romic until July.

## Downstream dependencies
None
