'''
this file runs a fortan file with a specified name and than deletes the executable
'''

import argparse
import os
import sys


# parser
parser = argparse.ArgumentParser(description='run fortran code')
parser.add_argument('filename', type=str, help='name of the file as name.f90 | name.f95')
args = parser.parse_args()


if __name__ == '__main__':
	path = os.getcwd()

	# check file
	if not os.listdir().__contains__(args.filename):
		print(f'file {args.filename} not found\n')
		sys.exit()

	# run
	os.system(f'gfortran {args.filename}')  # <-- use the gfortran compiler
	os.system('a.exe')

	# end
	print()
	os.system('rm a.exe')


