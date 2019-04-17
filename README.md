[![Build Status](https://travis-ci.com/fortran-gaming/oregon-trail-1975.svg?branch=master)](https://travis-ci.com/fortran-gaming/oregon-trail-1975)

# Oregon Trail 1975 & 1978


```basic
REM  MINNESOTA EDUCATIONAL COMPUTING CONSORTIUM STAFF
REM  PROGRAMMING REVISIONS BY DON RAWITSCH - 1975
REM  CURRENT VERSION - 3/27/75
```

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

Very lightly modified to work with modern platform-independent ANSI BASIC interpreter such as `bwbasic`



## Prereq
```sh
apt install bwbasic
```

or other favorite BASIC interpreter.

## Run

* 1975: `bwbasic oregon-trail-1975.bas`
* 1978: `bwbasic oregon-trail-1978.bas`


## Notes
Because the BASIC dialect used is old 1970s unstructured style, a very basic compiler is needed.
Because of this messy style, it is not worth fixing, but rather if someone wanted to preserve in a modern reinterpretation of classic style, I would suggest using Fortran with a module dedicated to registers for game state.

[1978 version](https://github.com/clintmoyer/oregon-trail) that's in this repo was carefully OCR'd and updated by @clintmoyer, tweaked by @scivision for ANSI BASIC compliance

