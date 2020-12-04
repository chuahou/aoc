#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
# Copyright (c) 2020 Chua Hou

# part 1
GREPS=""
for field in byr iyr eyr hgt hcl ecl pid; do
	GREPS+=" | grep $field"
done
eval "tr '\n' ' ' < input | sed 's/  /\n/g' $GREPS -c"
