#!/usr/bin/env python3
import sys
MAX_LEVEL = 7

if __name__ == '__main__':
	f = open(sys.argv[1], "r")
	line = ""
	for line in f:
		if line.startswith("<!--"):
			break
		length = min(MAX_LEVEL, len(line))
		level = line[0 : length].count('#')
		if line[0] == '#' and level > 0:
			new_level = MAX_LEVEL - level;
			line = line.replace('#'*level, '#'*new_level);
		print(line)
	f.close()
