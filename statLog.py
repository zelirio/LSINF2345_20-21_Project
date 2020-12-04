# -*- coding: utf-8 -*-
"""
Created on Tue Dec  1 10:07:58 2020

@author: Aurelien
"""

data = open("node.log").readlines()
inDegree = [dict() for i in range(180)]
MeaninDegree = [0 for i in range(180)]
SDinDegree = [0 for i in range(180)]
out = open("healer_deployment.data",'w')


for i in range(len(data)):
    data[i] = data[i][:-1].split(' ')
    round = int(data[i][2]) 
    neigh = data[i][1][:-2].split('],[')
    for l in neigh:
        parse = l.split(',')
        if parse[2] in inDegree[round]:
            inDegree[round][parse[2]] += 1
        else: 
            inDegree[round][parse[2]] = 1

for i in range(len(inDegree)):
    avg = 0
    std = 0
    for (key,value) in inDegree[i].items():
        avg += value
    avg /= len(inDegree[i])
    for (key,value) in inDegree[i].items():
        std += (value - avg) ** 2 
    std = (std / (len(inDegree[i])-1)) ** 0.5
    MeaninDegree[i] = avg
    SDinDegree[i] = std
    if i % 20 == 0:
        out.write(str(i) + " " + str(avg) + " " + str(std) + "\n")
    if i == 179:
        out.write(str(i) + " " + str(avg) + " " + str(std) + "\n")        
out.close()

    