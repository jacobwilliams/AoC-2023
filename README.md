![aoc2023](media/aoc2023.jpg)
============

[Advent of Code 2023](https://adventofcode.com/2023) with Modern Fortran.

[![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)](https://github.com/topics/fortran)
[![Build Status](https://github.com/jacobwilliams/AoC-2023/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/AoC-2023/actions)

## Compiling

All the cases can be compiled and run using the [Fortran Package Manager](https://fpm.fortran-lang.org).

### to run individual cases:

```
fpm run --profile release problem_01
```

### to run them all:

```
fpm run --profile release --all
```

## Current status

Problem  | Stars  | Solution | Runtime
--       | --     | --       | --
[1](https://adventofcode.com/2023/day/1)  | ⭐⭐ | [problem_01.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_01.f90) |  12 ms
[2](https://adventofcode.com/2023/day/2)  | ⭐⭐ | [problem_02.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_02.f90) |   4 ms
[3](https://adventofcode.com/2023/day/3)  | ⭐⭐ | [problem_03.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_03.f90) |   2 ms
[4](https://adventofcode.com/2023/day/4)  | ⭐⭐ | [problem_04.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_04.f90) |   5 ms
[5](https://adventofcode.com/2023/day/5)  | ⭐⭐ | [problem_05.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_05.f90) | 622 ms
[6](https://adventofcode.com/2023/day/6)  | ⭐⭐ | [problem_06.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_06.f90) |  45 ms
[7](https://adventofcode.com/2023/day/7)  | ⭐⭐ | [problem_07.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_07.f90) |  17 ms
[8](https://adventofcode.com/2023/day/8)  | ⭐⭐ | [problem_08.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_08.f90) |  11 ms
[9](https://adventofcode.com/2023/day/9)  | ⭐⭐ | [problem_09.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_09.f90) |  23 ms
[10](https://adventofcode.com/2023/day/10)| ⭐⭐ | [problem_10.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_10.f90) | 708 ms †
[11](https://adventofcode.com/2023/day/11)| ⭐⭐ | [problem_11.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_11.f90) |   1 ms
[12](https://adventofcode.com/2023/day/12)| ⭐⭐ | [problem_12.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_12.f90) + [problem_12b.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_12b.f90)  | 754 ms + 232 ms
[13](https://adventofcode.com/2023/day/13)| ⭐⭐ | [problem_13.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_13.f90) |   3 ms
[14](https://adventofcode.com/2023/day/14)| ⭐⭐  | [problem_14.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_14.f90) | 257 ms
[15](https://adventofcode.com/2023/day/15)| ⭐⭐  | [problem_15.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_15.f90) | 3 ms
[16](https://adventofcode.com/2023/day/16)| ⭐⭐ | [problem_16.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_16.f90) | 27 ms
[17](https://adventofcode.com/2023/day/17)| ⭐⭐  | [problem_17.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_17.f90) + [problem_17b.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_17b.f90) | minutes
[18](https://adventofcode.com/2023/day/18)| ⭐⭐  | [problem_18.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_18.f90) | 2 ms
[19](https://adventofcode.com/2023/day/19)| ⭐⭐ | [problem_19.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_19.f90) + [problem_19b.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_19b.f90) | 44 ms + 47 ms
[20](https://adventofcode.com/2023/day/20)| ☆☆  | [problem_20.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_20.f90) |
[21](https://adventofcode.com/2023/day/21)| ⭐☆  | [problem_21.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_21.f90) | 1 ms
[22](https://adventofcode.com/2023/day/22)| ⭐⭐  | [problem_22.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_22.f90) | 3588 ms
[23](https://adventofcode.com/2023/day/23)| ⭐⭐  | [problem_23.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_23.f90) | 3689 ms
[24](https://adventofcode.com/2023/day/24)| ⭐⭐  | [problem_24.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_24.f90) | 6 ms
[25](https://adventofcode.com/2023/day/25)| ⭐☆  | [problem_25.f90](https://github.com/jacobwilliams/AoC-2023/blob/master/app/problem_25.f90) | 1933 ms

† With OpenMP enabled (i.e, add `--flag "-fopenmp"` to the FPM call).

## Previous Years

 * [AoC-2020](https://github.com/jacobwilliams/AoC-2020)
 * [AoC-2021](https://github.com/jacobwilliams/AoC-2021)
 * [AoC-2022](https://github.com/jacobwilliams/AoC-2022)
 * [AoC-2023](https://github.com/jacobwilliams/AoC-2023)

 ## Documentation

 * The API documentation for the current ```master``` branch can be found [here](https://jacobwilliams.github.io/AoC-2023/).  This is generated by processing the source files with [FORD](https://github.com/Fortran-FOSS-Programmers/ford).
