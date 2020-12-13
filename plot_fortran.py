'''
the purpose of this program is to plot the data written in a text file with a given name; it opens it, use it and then deletes it.
More specifically, this code is called from within the fortran file network.f95.
'''



import os
import sys
import argparse
import matplotlib.pyplot as plt 

# parser
parser = argparse.ArgumentParser(description='plot fortran data')
parser.add_argument('filename', type=str, help='name of the file with the data as txt')
args = parser.parse_args()


if __name__ == '__main__':
	os.getcwd()

	# check file
	if not os.listdir().__contains__(args.filename):
		print('+ file with the data not found +')
		sys.exit()

	# get file
	with open(os.getcwd() + '\\' + args.filename, 'r') as f:
		filedata = f.read()

	# get data 
	raw = filedata.split('$')# <-- the variables are separated by $

	if raw.__len__() == 2:
		xraw, yraw = raw
		X = [eval(r) for r in [x for x in xraw.split(' ')] if r != '\n' and r != '']
		Y = [eval(r) for r in [y for y in yraw.split(' ')] if r != '\n' and r != '']

		# plot
		plt.plot(X, Y, label='weight over time')
		plt.title('STDP coupled neurons')
		plt.legend()
		plt.show()

	else:
		xraw, yraw, zraw = raw 
		X = [eval(r) for r in [x for x in xraw.split(' ')] if r != '\n' and r != '']
		Y = [eval(r) for r in [y for y in yraw.split(' ')] if r != '\n' and r != '']
		Z = [eval(r) for r in [z for z in zraw.split(' ')] if r != '\n' and r != '']

		# plot
		plt.subplot(211)
		start_t = 2000
		plt.plot(X[-start_t:], Z[-start_t:], label=f'network firing rate of the last {start_t}ms')
		plt.title('STDP network')
		plt.legend()

		plt.subplot(212)
		plt.plot(X, Y, '-r', label='average weight')

		plt.legend()
		plt.show()

	# remove file
	os.system('rm ' + args.filename)

