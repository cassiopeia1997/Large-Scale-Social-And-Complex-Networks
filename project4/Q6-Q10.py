#!/usr/bin/env python
# coding: utf-8

# In[8]:


import csv
import json
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from scipy.spatial import Delaunay


from igraph import *
import matplotlib.pyplot as plt
import json
import numpy as np
import os
import random


# In[9]:


directory = os.path.abspath("data/")

graph_data = pd.read_csv(directory + '/san_francisco-censustracts-2017-4-All-MonthlyAggregate.csv')
graph_data = graph_data.drop(['standard_deviation_travel_time', 'geometric_mean_travel_time', 'geometric_standard_deviation_travel_time'], axis=1)
graph_data = graph_data.drop(graph_data[graph_data.month != 12].index)
dec_data = graph_data.drop(['month'], axis=1)

dec_array = np.asarray(dec_data) # idk numpy seems easier to work with
existing_paths = {}
existing_locations = np.unique(np.concatenate([dec_array[:,0],dec_array[:,1]]))
counter = 0
for row in dec_array:
    locations = tuple(np.sort(row[0:2]))
    if locations in existing_paths:
        existing_paths[locations].append(row[2])
        counter += 1
    else:
        existing_paths[locations] = [row[2]]
        
with open(directory + '/december_data.txt','w') as f:
    for locations in existing_paths:
        string = '{} {} {:.2f}\n'.format(int(locations[0]),int(locations[1]),np.mean(existing_paths[locations]))
        f.write(string)


# In[ ]:


location_dictionary = {}

for entry in geo_data:
    data = {}
    # Retrieve coordinate list and compute mean coordinates
    coord_list = entry['geometry']['coordinates'][0][0]
    mean_coord = np.mean(np.asarray(coord_list),axis=0)
    movement_id = entry['properties']['MOVEMENT_ID']

    data['coord_list'] = coord_list
    data['mean_coord'] = mean_coord
    data['display_name'] = entry['properties']['DISPLAY_NAME']
    location_dictionary[movement_id] = data


# # Question 6

# In[ ]:


uber_graph = Graph.Read(directory + '/december_data.txt', format='ncol', directed=False)

gcc = uber_graph.components().giant()
summary(gcc)
summary(uber_graph)

print("We can see 7 nodes were removed")

Graph.write_ncol(gcc, directory + '/uber_gcc.txt')


# # Question 7

# In[ ]:


mst = gcc.spanning_tree(weights = gcc.es["weight"])
vertices = mst.vs()
summary(mst)

print('-'*50)

rand_n = []
for x in range(3):
    rand_n.append(random.randint(0,len(mst.es())-1))

count = 0
rand_e = []
for e in mst.es():
    st = e.tuple
    if count < 10:
        print(location_dictionary[vertices[st[0]]['name']]['display_name'],'---',location_dictionary[vertices[st[1]]['name']]['display_name'])
    
    if count in rand_n:
        rand_e.append(location_dictionary[vertices[st[0]]['name']]['display_name'])
        rand_e.append(location_dictionary[vertices[st[1]]['name']]['display_name'])
    count += 1


# # Question 8

# In[ ]:


def check_if_satisfy():
    current_set = ()
    indices = []
    weights = []
    
    for index in range(0,3): #generate 3 indices
        indices.append(random.randint(0,max_index))        
    current_set = tuple(sorted(indices))
    
    while(current_set in combined_set):
        for index in range(0,3): #generate 3 indices
            indices.append(random.randint(0,max_index))        
        current_set = tuple(sorted(indices))
        
    for e in current_set:
        weights.append(gcc_es[e]["weight"])
    
    if weights[0] < weights[1] + weights[2] and weights[1] < weights[0] + weights[2] and weights[2] < weights[0] + weights[1]:
        return True, weights, current_set
    else:
        return False, weights, current_set



sample_triangles = 1000
max_index = len(gcc.vs()) - 1  # used for generating index
gcc_vertices = gcc.vs()
gcc_es = EdgeSeq(gcc)

combined_set = set()
satisfied = 0

count = 0
while count < sample_triangles:
    #randomly generated 1000 triangles
                       
    for i in range(0,3):
        satisfy, weights, current_set = check_if_satisfy()
        if satisfy:
            break
                       
    combined_set.add(current_set)
    
    print('='*50,'triangle',count+1)
   
    edges = []
    start_nodes = []
    end_nodes = []
    for e in current_set:
        edges.append(gcc_es[e])
        
        source_vertex = gcc.vs[gcc_es[e].source]
        target_vertex = gcc.vs[gcc_es[e].target]
        
        start_nodes.append(location_dictionary[source_vertex['name']]['display_name'])
        end_nodes.append(location_dictionary[target_vertex['name']]['display_name'])
    
    for i in range(0, 3):
        print('weight:', weights[i],'edge ' + str(i+1) + ':', start_nodes[i], '---', end_nodes[i])
    
    if satisfy:
        satisfied += 1
        print('traingle satisfied')
    else:
        print('triangle not satisfied')
        
    count += 1  
    
print('percent:', str(satisfied / sample_triangles))


# # Question 9

# In[ ]:


directory = os.path.abspath("data/")

uber_graph = Graph.Read(directory + '/december_data.txt', format='ncol', directed=False)
gcc = uber_graph.components().giant()

def create_a_walk(mst, walk, visited, mst_vset):
    current_node = walk[-1]
    
    
    if len(walk) > 1 and current_node == walk[0]: return True
    else:
        edges = mst.es.select(_between = ([current_node],mst_vset))
    
        for edge in edges:
            # add edge if not visited
            if not edge in visited:
                visited.add(edge)
                node_tuple = edge.tuple

                if node_tuple[0] == current_node:
                    #add next node
                    next_node = node_tuple[1]
                else:
                    #add current node
                    next_node = node_tuple[0]

                walk.append(next_node)
                next_walk = create_a_walk(mst, walk, visited, mst_vset)
                if next_walk:
                    return True
                else:
                    walk.pop()
                    visited.remove(edge)
                    
        return False
            



def eulerian_walk(mst, v, mst_vset):
    edges = mst.es.select(_source_in = [v])
    
    if len(edges) == 0: return [v] #empty walk
    else:
        # do eulerian walk
        walk = [v]
        visited = set()
        create_a_walk(mst, walk, visited, mst_vset)
        for i in range(len(walk) - 1):
            walk_edges = mst.es.select(_between = ([walk[i]], [walk[i+1]]))
            mst.delete_edges(walk_edges[0])
            
        results = []
        for node in walk:
            next_ewalk = eulerian_walk(mst, node, mst_vset)
            for e in next_ewalk:
                results.append(e)
                
        return results
            
        
        
    


def approximate_algorithm(g):
    # find the minimum spanning tree T
    mst = g.spanning_tree(weights = gcc.es["weight"])
    edges = mst.es()
    
    # Create a multigraph G by using two copies of each edge of T
    # turn the undirected graph into a directed one with edges running both ways between nodes
    edge_list = []
    for edge in edges:
        edge_list.append(edge.tuple)
        
    mst_temp = mst.as_undirected() # needs to be undirected regardless
    mst_temp.add_edges(edge_list)
    
    mst_double = mst_temp.as_undirected()

    # Find an Eulerian walk of G and an embedded tour.
    e_walk = eulerian_walk(mst_double, 0, mst_double.vs())
    
    # do the approximation algorithm
    path = []
    visited = set()
    for i in e_walk:
        if not i in visited:
            path.append(i)
            visited.add(i)
            
    total_weight = 0
    final_path = []
    for i in range(len(path) - 1):
        i_1 = path[i]
        i_2 = path[i + 1]
#         print(i_1,i_2)
        
        edges = g.es.select(_between = ([i_1], [i_2]))
        if len(edges) > 0:
            total_weight += edges[0]['weight']
            final_path.append(i_1)
#             final_path.append(i_2)
        else:
            # use shortest path algorithm if no valid edge
            total_weight += gcc.shortest_paths([i_1], [i_2], weights = gcc.es()['weight'])[0][0]

    return mst, total_weight, final_path 


# # Question 10

# In[ ]:


plt.figure(figsize=(25,20))

plt.plot(longitude, latitude)
plt.title('Trajectory of TSP')
plt.xlabel('Degrees Longitude')
plt.ylabel('Degrees Latitude')
plt.show()

