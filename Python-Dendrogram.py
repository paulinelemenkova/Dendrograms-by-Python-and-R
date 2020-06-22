#!/usr/bin/env python
# coding: utf-8
import os
import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
from scipy.cluster import hierarchy
from scipy.cluster.hierarchy import dendrogram, linkage

os.chdir('/Users/pauline/Documents/Python')
df = pd.read_csv("Tab-Morph.csv")
df = df.set_index('profile')
del df.index.name

# calculate distance between each sample
Z = hierarchy.linkage(df, 'average')

# plotting
dendrogram(Z, labels=df.index, leaf_rotation=0, orientation="left",
           distance_sort='ascending')
plt.title('Hierarchical cluster dendrogram for the geomorphic similarity \nof the 25bathymetric profiles, Mariana Trench',
          fontsize=10, fontfamily='sans-serif')

# visualizing and saving
plt.tight_layout()
plt.savefig('plot_Dendro.png', dpi=300)
plt.show()
