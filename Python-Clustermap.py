#!/usr/bin/env python
# coding: utf-8
import os
import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
import seaborn as sb

sb.set(style='white')
sb.set(color_codes=True)
sb.set_context('paper')

os.chdir('/Users/pauline/Documents/Python')
df = pd.read_csv("Tab-Bathy.csv")

# define variables and plotting
g = sb.clustermap(df, cmap="mako", robust=True)
rotation = 45
for i, ax in enumerate(g.fig.axes):   ## getting all axes of the fig object
     ax.set_xticklabels(ax.get_xticklabels(),
                        rotation = rotation
                        )

g.fig.suptitle('Mariana Trench: Clustermap of the bathymetric observations. \nDataset: 25 cross-section profiles, 518 observations in each. \nMethod: ignored outliers in colormap limits') 
plt.subplots_adjust(bottom=0.20,top=0.90, right=0.90, left=0.10)

# printing and saving
plt.savefig('plot_Clust3.png', dpi=300)
plt.show()
