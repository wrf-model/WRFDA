#!/usr/bin/env python

# This script takes the output "cost_fn" file from a WRFDA run and plots the minimization progress.
# If there were multiple outer loops, they are plotted as separate lines on the same plot

import matplotlib.pyplot as plt
import numpy as np
import csv

#Read in file 'cost_fn' as space-delimited data
with open('cost_fn') as f:
    reader = csv.reader(f, delimiter=" ", skipinitialspace=True)
    Jdata = list(reader)

#Number of outer loops is first value in final row
outerloops= int(Jdata[len(Jdata)-1][0])

cost_fn_values=np.zeros(len(Jdata)-2)

#Create 1-d array of cost function values from read-in list
for i in range (2,len(Jdata)):
    cost_fn_values[i-2] = Jdata[i][3]

#print cost_fn_values

# Loop through cost_fn_values, starting a new plot for each outer loop
# We know a new outer loop is started when the cost function exactly repeats
x=1
for i in range (0,outerloops):
    y = x-1
    while cost_fn_values[x] != cost_fn_values[x-1]:
        x += 1

    plt.plot(cost_fn_values[y:x],label="Loop "+str(i+1))
    # y starts as x-1, so we skip two values at start of new outerloop
    x += 2

plt.legend(loc='upper right')
plt.ylabel('Total cost function')
plt.xlabel('Minimization step')
plt.savefig('cost_fn.pdf',format='pdf')


