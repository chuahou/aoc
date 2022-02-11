// SPDX-License-Identifier: MIT
// Copyright (c) 2022 Chua Hou

#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define GRID_SIZE_X 10
#define GRID_SIZE_Y 10

#define MIN(a, b) (a < b ? a : b)
#define MAX(a, b) (a > b ? a : b)

typedef uint8_t energies_t[GRID_SIZE_X][GRID_SIZE_Y];

void assert(bool condition, const char *msg)
{
	if (!condition) fprintf(stderr, "Assertion failed: %s\n", msg);
}

void flash(energies_t energies, size_t x, size_t y, uint64_t *count)
{
	assert(energies[x][y] == 10, "Invalid flash");
	(*count)++;
	for (size_t x_ = MAX((int)x - 1, 0); x_ <= MIN(x + 1, GRID_SIZE_X - 1); x_++) {
		for (size_t y_ = MAX((int)y - 1, 0); y_ <= MIN(y + 1, GRID_SIZE_Y - 1); y_++) {
			energies[x_][y_]++;
			if (energies[x_][y_] == 10) flash(energies, x_, y_, count);
		}
	}
}

void step(energies_t energies, uint64_t *count)
{
	for (size_t i = 0; i < GRID_SIZE_X; i++) {
		for (size_t j = 0; j < GRID_SIZE_Y; j++) {
			energies[i][j]++;
			if (energies[i][j] == 10)
				flash(energies, i, j, count);
		}
	}
	for (size_t i = 0; i < GRID_SIZE_X; i++) {
		for (size_t j = 0; j < GRID_SIZE_Y; j++) {
			energies[i][j] = energies[i][j] > 9 ? 0 : energies[i][j];
		}
	}
}

void print_energies(energies_t energies)
{
	for (size_t i = 0; i < GRID_SIZE_X; i++) {
		for (size_t j = 0; j < GRID_SIZE_Y; j++) {
			if (energies[i][j] > 9) putchar('X');
			else putchar(energies[i][j] + '0');
		}
		putchar('\n');
	}
	putchar('\n');
}

void day11(char *input)
{
	energies_t energies;
	for (size_t i = 0; i < GRID_SIZE_X; i++) {
		for (size_t j = 0; j < GRID_SIZE_Y; j++) {
			energies[i][j] = input[i * 11 + j] - '0';
		}
		assert(input[i * 11 + 10] == '\n', "Invalid input");
	}

	uint64_t count = 0;
	for (int i = 0; i < 100; i++) step(energies, &count);
	printf("%lu\n", count);
}
