# myfs

The R package includes useful functions created by hosscine.

## Install

```
devtools::install_github("hosscine/myfs")
```

## All Functions

### plot matrix
- **`image2(X, col.rainbow = F, col.gray0 = F)`**
  - the new `image` function of more colorful, gridable and filling 0 valued cells to gray color.
- **`databar(X, grid = T)`**
  - the microsoft excel liked databar plot.
- `plotLines(X, legend.locale = "topleft", ...)`
  - plot matrix with regarding each rows as a line.

### plotting support
- `setMargin(top.4 = 4, left.4 = 4, bottom.5 = 5, right.2 = 2)`
  - set plot margine with default `c(4, 4, 5, 2)`.
- `setMarginZero()`
  - set plot margine to all zeros.
- `resetMfrow()` 
  - reset splitting of plot window by executing `par(mfrow = c(1, 1))`.
- `keywait()`
  - replace of `par(wait=T)`.

### matrix and vector operation
#### for vector
- `runifN(n,min=1,max=10)`
  - the `runif` function returns int value.
- `vnorm(x)`
  - compute norm of vector.
- `order2(x, decreasing = F)`
  - compute `order` with distinguish same orderd values.
- `euclidean(x, y)`
  - compute euclidean distance.
- `angle(x, y)`
  - compute angle between 2-dimensional vectors.
  
#### for matrix
- `rownorm(x)`
  - compute norm of each rows of matrix.
- **`rowMinus(X, a)`**
  - compute `X - a` for each rows.
- **`rowTimes(X, a)`**
  - compute `X * a` for each rows.
- `replaceNA(X, replace = 0)`
  - replace NA.
- `rank.matrix(X)`
  - compute rank of all values of matrix.
- **`order.col(X, decreasing = F, ignore0 = F)`**
  - compute `order` for each rows.
- **`sort_col(X, decreasing = F, ignore0 = F)`**
  - compute `sort` for each rows.
- `padding.matrix(X, size = 1, replace = NULL)`
  - expand size of matrix like 2d-kernel.
- `matlist(lst, nrow=length(lst), ncol=length(lst[[nrow]]), rname=names(lst), cname=names(lst[[nrow]]))`
  - convert a list of vectors nested by lists to the matrix.

### assertion
- `is.prob(x)`
  - assert a value is probalistic or not.
- `is.range(x)`
  - assert a 2d-vector indicates a range or not.

### debug support
- **`debugText(...)`**
  - the `print` function ables to multi arguments.
- `print.noattr(x, keeps = c('names', 'row.names', 'class', 'dim', 'dimnames'), ...)`
  - print without unique attributes.

### handling ellipsis `...`
- **`overwriteEllipsis(..., append = NULL, warn = T)`**
  - fix a value of ellipsis.
- **`softwriteEllipsis(..., append = NULL, warn = F)`**
  - add a value to ellipsis if not a conflict occurs.

### directory support
- **`todaywd()`**
  - move to today directory. (default is "~/R/実験データ/YYYY_MM_DD")
- `is.todayDirectory()`
  - check current is today directory.
- `subwd(name, current = F)`
  - make sub directory into today directory.
- `savedDir(pass)`
  - change default today directory.
- **`savepng(name, current = F, scale = c(450, 500))`**
  - save plot to today directory.
- `savepngTitle(name, current = F, scale=c(450, 500))`
  - save plot to today directory with main title.
- `saveEnvironment()`
  - save global environment as anonymas file ".Rdata".
- **`setwdProject()`**
  - set working directory to project directory
- `dir.create.deep()`
  - create directory for nested path.
  
### Rnotebook support
- `preparePrintify()`
  - print Rnotebook html file with arranging code chuncks.

### pipe operater
- **`%.%`**
  - pipe without passing pipe before result to pipe after first argument.
- **`%.T%`**
  - pipe by `%.%` with tee.
  
### other utilities
- `saveEnvironment()`
  - save .globalEnv without quitting R session.
- `setwdProject()`
  - set working directory to default of Rstudio project.
- `eval_timer()`
  - measure the elapsed time of evaluation and return the result.

## Licence

[MIT](https://github.com/tcnksm/tool/blob/master/LICENCE)

## Author

[hosscine](https://github.com/hosscine)
