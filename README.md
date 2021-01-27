# Oregon Trail 1975 & 1978

[![Actions Status](https://github.com/fortran-gaming/oregon-trail-1975/workflows/ci_basic/badge.svg)](https://github.com/fortran-gaming/oregon-trail-1975/actions)

```basic
REM  MINNESOTA EDUCATIONAL COMPUTING CONSORTIUM STAFF
REM  PROGRAMMING REVISIONS BY DON RAWITSCH - 1975
REM  CURRENT VERSION - 3/27/75
```

Very lightly modified to work with modern platform-independent ANSI BASIC interpreter such as `bwbasic`

## Prereq

```sh
apt install bwbasic
```

or other favorite BASIC interpreter.

## Run

* 1975: `bwbasic oregon-trail-1975.bas`
* 1978: `bwbasic oregon-trail-1978.bas`

## Provenance

### 1978 version

From
[Creative Computing 1978](https://ia800307.us.archive.org/3/items/creativecomputing-1978-05/Creative_Computing_v04_n03_1978_May-June.pdf)
page 132ff.

```basic
REM The program that follows is a reconstruction
REM of the Oregon Trail game written for HP time-shared
REM BASIC by Don Rawitsch and Bill Heinemann and Paul Dillenberger
REM in 1971. Its source is an updated version published in the
REM July-August 1978 issue of Creative Computing.


10 REM PROGRAM NAME - 0REGON        VERSION:01/01/78
20 REM ORIGINAL PROGRAMMING BY BILL HEINEMANN - 1971
30 REM SUPPORT RESEARCH AND MATERIALS BY DON RAVITSCH
40 REM MINNESOTA EDUCATIONAL COMPUTING CONSORTIUM STAFF
50 REM CDC CYBER 70/73-26 BASIC 3-1
```

* OCR'd by [@clintmoyer](https://github.com/clintmoyer/oregon-trail),
* tweaked by @scivision for ANSI BASIC compliance,

Further corrected by pull request contributors:

* @pau101
* @philjonas

### 1975 version

https://www.filfre.net/misc/oregon1975.bas

## Notes

The BASIC dialect used is old 1970s unstructured style, which needed tweaks to run in a contemporary BASIC interpreter.
To preserve in a modern reinterpretation of classic style, consider Fortran with a module dedicated to registers for game state.
