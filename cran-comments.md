## R CMD check results

0 errors | 0 warnings | 0 notes

## Comments

This release fixes the WARNING on the CRAN checks of blockr.dag 0.1.2
(https://cran.r-project.org/web/checks/check_results_blockr.dag.html):
"Missing or unexported object: 'blockr.dock::show_panel'". blockr.dag no
longer uses that removed function.

This version depends on g6R (>= 0.6.5), which has been submitted to CRAN
ahead of this package.
