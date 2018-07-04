import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

x = np.array([[0, 0, 255], [255, 255, 0], [0, 255, 0]])
plt.imshow(x, interpolation='nearest')
plt.show()

np.random.choice([0, 255], 4*4, p=[0.1, 0.9]).reshape(4, 4)

def addGlider(i, j, grid):
	""" adds a glider with top left cell at (i, j)"""
	glider = np.array(	[[0, 0, 255],
				[255, 0, 255],
				[0, 255, 255]])
	plt.imshow(glider, interpolation='nearest')
	plt.show()
	grid[i:i+3, j:j+3] = glider
	plt.imshow(glider, interpolation='nearest')
	plt.show()
N = 16

grid = np.zeros(N*N).reshape(N, N)
addGlider(1, 1, grid)
