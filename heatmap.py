# -*- coding: utf-8 -*-
"""
Created on Tue Jul 24 10:45:27 2018

@author: AiD
"""

import os
os.getcwd()
os.chdir(r'C:/Users/aid/Desktop/Heatmap')
# os.chdir(r'/Users/ai/Desktop/Forum Python/Heat map')

import pandas as pd
dat = pd.read_csv("20180725-SurveyExport.csv")
list(dat)

# =============================================================================
# Data cleaning part - Transform the data, expand the rows and create new columns
# =============================================================================

map_dat = dat[['Response ID', 'Select a place please']]
map_dat.columns = ['Response ID', 'output'] # name could be changed

# split concentrated data from one line into multiple lines 
import re
new_text_col = []
l = []
for t in map_dat['output']:
    re_t = re.findall(r': (.*)\r\n', t)
    l.append(len(re_t))
    new_text_col = new_text_col + re_t

# Create a new column of the index to match the output
import numpy as np
trans_data = pd.DataFrame(
        {'Response ID': np.repeat(map_dat['Response ID'], l), 
         'output': new_text_col
                })
trans_data.reset_index(drop=True, inplace=True)

# get x, y, x_percent, y_percent, x_grid, y_grid, comment out of the data
answer = []
x = []
y = []
xPercent = []
yPercent = []
gridX = []
gridY = []
comment = []

for t in trans_data["output"]:
    answer += re.findall(r'answer - (\d*)', t)
    x += re.findall(r'x - (\d*)', t)
    y += re.findall(r'y - (\d*)', t)
    xPercent += re.findall(r'xPercent - (\d*)', t)
    yPercent += re.findall(r'yPercent - (\d*)', t)
    gridX += re.findall(r'gridX - (\d*)', t)
    gridY += re.findall(r'gridY - (\d*)', t)
    comment += re.findall(r'comment - (.*)', t)
    
# combine the new columns back to trans_data
trans_data["answer"] = pd.Series(answer)
trans_data["x"] = pd.Series(x)
trans_data["y"] = pd.Series(y)
trans_data["xPercent"] = pd.Series(xPercent)
trans_data["yPercent"] = pd.Series(yPercent)
trans_data["gridX"] = pd.Series(gridX)
trans_data["gridY"] = pd.Series(gridY)
trans_data["comment"] = pd.Series(comment)

# =============================================================================
#  Plot those coordinates on the canvas
# =============================================================================
trans_data_like = trans_data.loc[trans_data['answer'] == '10001']
trans_data_dislike = trans_data.loc[trans_data['answer'] == '10002']


# the origin for the graph is on the top-left corner
# scatter plot
import matplotlib.pyplot as plt
# load original pic, could be changed to other floor plans
im = plt.imread(r'IPDCollaboration.jpg')
# transform x and y from pd.series to list of numbers
x = list(np.array(list(trans_data_like["gridX"])).astype(int)*im.shape[1]/100)
y = list(np.array(list(trans_data_like["gridY"])).astype(int)*im.shape[0]/100)
implot = plt.imshow(im, aspect = 'auto')
# c = '' is for the format of color and s = defines the size of the marker
plt.scatter(x, y, c = 'r', s = 10)
# save the fig
plt.savefig(r'C:\Users\aid\Desktop\Heatmap\fig_update.png', dpi = 300)
plt.close

# =============================================================================
# # try colored dots by density
# =============================================================================
from scipy.stats import gaussian_kde
# calculate the point density
xy = np.vstack([x, y])
z = gaussian_kde(xy)(xy)

# density point plot, Sort the points by density, so that the densest points are plotted last
idx = z.argsort()
x_d, y_d, z = np.array(x)[idx], np.array(y)[idx], z[idx]
im = plt.imread(r'C:\Users\aid\Desktop\Heatmap\IPDCollaboration.jpg')
implot = plt.imshow(im, aspect = 'auto')
plt.scatter(x_d, y_d, c = z, s = 50, edgecolor = '', cmap = 'inferno')
# cmap stands for color map, more info could be found @ https://matplotlib.org/examples/color/colormaps_reference.html
plt.colorbar()
plt.savefig(r'C:\Users\aid\Desktop\Heatmap\fig_like_densitycol.png', dpi = 300)
plt.close

# =============================================================================
# # density contour graph
# =============================================================================
import numpy as np
from scipy.stats import kde
nbins = 100
k = kde.gaussian_kde(xy)
#xi, yi = np.mgrid[np.array(x).min():np.array(x).max():nbins*1j, np.array(y).min():np.array(y).max():nbins*1j]
xi, yi = np.mgrid[0:im.shape[1]:nbins*1j, 0:im.shape[0]:nbins*1j]
zi = k(np.vstack([xi.flatten(), yi.flatten()]))
im = plt.imread(r'C:\Users\aid\Desktop\Heatmap\IPDCollaboration.jpg')
implot = plt.imshow(im, aspect = 'auto')
plt.contourf(xi, yi, zi.reshape(xi.shape), 100, cmap = 'Reds', vmin = 0, alpha = 0.3)
plt.colorbar()
plt.savefig(r'C:\Users\aid\Desktop\Heatmap\fig_like_contour2.png', dpi = 300)
plt.close


# =============================================================================
# # try new mpl-scatter-density package
# # Not good at all, may need a lot of points to see the density plot
# =============================================================================
import mpl_scatter_density
im = plt.imread(r'IPDCollaboration.jpg')
implot = plt.imshow(im, aspect = 'auto')
fig = plt.figure()
fig.add_subplot(1, 1, 1, projection = 'scatter_density')
fig.scatter_density(x, y, color = 'red')
fig.colorbar(label = 'number of points per pixel')
fig.savefig(r'fig_like_scatter_density.png', dpi = 300)
fig.close


# =============================================================================
# # Another try for the halo effect
# # Blend transparency with color in 2-D images
# =============================================================================
import numpy as np
import matplotlib.pyplot as plt

def normal_pdf(x, mean, var):
    return np.exp(-(x - mean)**2 / (2*var))

# Generate the space in which the blobs will live
xmin, xmax, ymin, ymax = (0, im.shape[1], 0, im.shape[0])
n_bins = 100
xx = np.linspace(xmin, xmax, n_bins)
yy = np.linspace(ymin, ymax, n_bins)

# Generate the blobs.
# x and y are lists of numbers
x = list(np.array(list(trans_data_like["gridX"])).astype(int)*im.shape[1]/100)
y = list(np.array(list(trans_data_like["gridY"])).astype(int)*im.shape[0]/100)
xy = np.vstack([x, y])
var = 20000 # variation could be changed

gauss_x = []
for xs in xy[0]:
    gauss_x.append(normal_pdf(xx, xs, var)[:])

gauss_y = []
for ys in xy[1]:
    gauss_y.append(normal_pdf(yy, ys, var)[:])

weights = []
for xxx, yyy in zip(gauss_x, gauss_y):
    weights.append(np.array(np.meshgrid(xxx, yyy)).prod(0))

# First we'll plot these blobs using only ``imshow``.
# sum all the ndarrays stored in the weights so that we could plot all the points in one graph
new_w = sum(weights)
# still find a way to merge with the floor plan and I believe it is feasible
# im = plt.imread(r'IPDCollaboration.jpg')
# implot = plt.imshow(im, aspect = 'auto')
implot = plt.imshow(im, aspect = 'auto')
plt.imshow(new_w, extent=(xmin, xmax, ymin, ymax), cmap = 'Reds', alpha = 0.7)



