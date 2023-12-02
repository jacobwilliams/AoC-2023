# AoC-2023

[Advent of Code 2023](https://adventofcode.com/2023) with Fortran

[![Build Status](https://github.com/jacobwilliams/AoC-2023/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/AoC-2023/actions)


### to run individual cases:

```
fpm run problem_01
```

etc.

### to run them all:

```bash
    fpm build --profile release
    rm -rf ./build/gfortran_*/app/*.log
    for filename in ./build/gfortran_*/app/*; do
        echo ""
        echo "---------- $filename ----------"
        time ./$filename
        echo ""
    done
```