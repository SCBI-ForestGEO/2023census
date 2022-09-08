# Census methods

- use ArcGIS app on iPads
- CI system in GitHub to run any tests that cannot be done using ArcGIS app ([issue #2](https://github.com/SCBI-ForestGEO/2023census/issues/2))
- need to integrate with mortality census ([issue #1](https://github.com/SCBI-ForestGEO/2023census/issues/1))

## tests to include in ArcGIS app vs GitHub ([issue #3](https://github.com/SCBI-ForestGEO/2023census/issues/3))
There are presumably some checks that could be done in either one. It's preferable to have quick checks in the field, but the CI system would be useful for ones that are hard to code.

### ArcGIS app
- We can check for DBH discrepancies - not sure how detailed we can make that just yet, but we can certainly set alerts for when trees 'shrink'.

### GitHub
- probably better for tag errors 
- useful if we want to design any macrospcopic checks (e.g., mortality rates within expected bounds? size distributions of abundance and growth rate reasonable?)
