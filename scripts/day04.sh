#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
# Copyright (c) 2020 Chua Hou

# part 1
GREPS=""
for field in byr iyr eyr hgt hcl ecl pid; do
	GREPS+=" | grep $field"
done
eval "tr '\n' ' ' < input/day04.txt | sed 's/  /\n/g' $GREPS -c"

# part 2
tr '\n' ' ' < input/day04.txt | sed 's/  /\n/g' | \
	grep 'byr:\(19[2-9][0-9]\|200[0-2]\)' | \
	grep 'iyr:20\(1[0-9]\|20\)' | \
	grep 'eyr:20\(2[0-9]\|30\)' | \
	grep 'hgt:\(1\([5-8][0-9]\|9[0-3]\)cm\|\(59\|6[0-9]\|7[0-6]\)in\)' | \
	grep 'hcl:#[0-9a-f]\{6\}' | \
	grep 'ecl:\(amb\|blu\|brn\|gry\|grn\|hzl\|oth\)' | \
	grep 'pid:[0-9]\{9\}\([^0-9]\|$\)' -c
