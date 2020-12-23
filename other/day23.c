// SPDX-License-Identifier: MIT
// Copyright (c) 2020 Chua Hou

#include <stdio.h>
#include <stdlib.h>

int main(void)
{
	// le_mutable_structure[i] = j means the i-th cup is followed by cup j
	int le_mutable_structure[1000000 + 1];

	FILE *file = fopen("input/day23.txt", "r");
	int first = (int) (fgetc(file) - '0');
	int curr = first;
	char c;
	c = fgetc(file);
	do {
		int next = (int) (c - '0');
		le_mutable_structure[curr] = next;
		curr = next;
	} while ((c = fgetc(file)) != '\n');
	fclose(file);

	for (int next = 10; next <= 1000000; next++) {
		le_mutable_structure[curr] = next;
		curr = next;
	}
	le_mutable_structure[curr] = first;

	curr = first;
	for (int i = 0; i < 10000000; i++) {
		int pick1 = le_mutable_structure[curr];
		int pick2 = le_mutable_structure[pick1];
		int pick3 = le_mutable_structure[pick2];
		le_mutable_structure[curr] = le_mutable_structure[pick3];

		int dest = curr == 1 ? 1000000 : curr - 1;
		while (dest == pick1 || dest == pick2 || dest == pick3) {
			dest = dest == 1 ? 1000000 : dest - 1;
		}
		le_mutable_structure[pick3] = le_mutable_structure[dest];
		le_mutable_structure[dest] = pick1;

		curr = le_mutable_structure[curr];
	}

	unsigned long star1 = le_mutable_structure[1];
	unsigned long star2 = le_mutable_structure[star1];
	printf("%lu\n", star1 * star2);

	return 0;
}
