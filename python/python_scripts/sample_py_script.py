library(reticulate)


# Python Data Science Packages installed as below ---- 

# reticulate::py_install("pandas")
# py_install("matplotlib")
# py_install("seaborn")
# py_install("scikit-learn")


# Basic Pandas data types --------------

py_dict_new = {'name' : 'Arnab Panja', 'species' : 'Human'}

py_dict_new


def find_avg_new(a, b):
  return (a+b)/2


find_avg_new(25, 75)


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt 
import seaborn as sns



data = np.random.randn(2, 3)

data

data * 10

data + data

obj = pd.Series([4, 7, 5, -3])

obj

# create a sample matplotlib plot ----------
sample_plot1 = plt.plot(np.random.randn(50).cumsum())


# show a sample matplotlib plot ----------
plt.show(sample_plot1)


# show a sample seaborn plot 

sns.set_theme(style="whitegrid")

# Load the example diamonds dataset
diamonds = sns.load_dataset("diamonds")

# Draw a scatter plot while assigning point colors and sizes to different
# variables in the dataset
f, ax = plt.subplots(figsize=(1, 1))
sns.despine(f, left=True, bottom=True)
clarity_ranking = ["I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF"]
sample_sns_plot = sns.scatterplot(
                  x="carat", 
                  y="price",
                  hue="clarity", 
                  size="depth",
                  palette="ch:r=-.2,d=.3_r",
                  hue_order=clarity_ranking,
                  sizes=(1, 8), 
                  linewidth=0,
                  data=diamonds, 
                  ax=ax)

plt.show(sample_sns_plot)
