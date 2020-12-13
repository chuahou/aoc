#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
# Copyright (c) 2020 Chua Hou

# part 1
for grp in $(awk NF=NF RS= OFS= input/day06.txt); do
	tr '\n' ' ' <<< $grp | sed 's/./&\n/g' | sort | uniq | sed '/^ $/d' | wc -l
done | paste -sd+ | bc
