import os
from io import open
from conllu import parse
import networkx as nx
from operator import itemgetter
import random


directory = "./SUD"                   # directory containing the UD scheme tree files in CONLLU format
sud_files = []
for root, dirs, files in os.walk(directory):
    for file in files:
        if file.endswith('train.conllu'):
            fullpath = os.path.join(root, file)
            sud_files.append(fullpath)            # creates a list of path of all files (file of each language) from the directory

print(sud_files)

results = open("DL-distribution.csv","w")
results.write("Lang\tSent_id\tS_length\tDependency\tDL\n")
results.close()

DL_art = []
for i in sud_files:                  # reads file of each language one by one
    lang = str(i).replace("./SUD", "")
    lang=lang.replace("-sud-train.conllu", "")            # lang variable stores the language code
    lang=lang.replace("-sud-test.conllu", "")
    data_file = open(str(i),'r',encoding='utf-8').read()
    sentences = []
    sentences = parse(data_file)                         # parses the CONLLU format
    print(lang)
    sent_id =0

    DL_lang = []
    for sentence in sentences[0:]:
        sent_id+=1
        #print(sent_id)
        if sent_id<15000:
            tree =   nx.DiGraph()                              # An empty directed graph (i.e., edges are uni-directional)
            for nodeinfo in sentence[0:]:                    # retrieves information of each node from dependency tree in UD format
                entry=list(nodeinfo.items())
                if len(entry)>7 and (not entry[7][1]=='punct'):
                    tree.add_node(entry[0][1], form=entry[1][1], lemma=entry[2][1], upostag=entry[3][1], xpostag=entry[4][1], feats=entry[5][1], head=entry[6][1], deprel=entry[7][1], deps=entry[8][1], misc=entry[9][1])                #adds node to the directed graph
                

            ROOT=0
            tree.add_node(ROOT)                            # adds an abstract root node to the directed graph

            for nodex in tree.nodes:
                if not nodex==0:
                    if tree.has_node(tree.nodes[nodex]['head']):                                         # to handle disjoint trees
                        tree.add_edge(tree.nodes[nodex]['head'],nodex,drel=tree.nodes[nodex]['deprel'])       # adds edges as relation between nodes

            #print(tree.edges)
            
            s_length = len(tree.edges)
            verb_count = 0
            core = []

            if s_length>2 and s_length<30:
                art_tree = nx.DiGraph()
                node_list = list(tree.nodes)
                random.shuffle(node_list)
                reordered_nodes = node_list
                
                for node in reordered_nodes:
                    if not node==0:
                        art_tree.add_node(node)

                for node in art_tree.nodes:
                    if tree.has_node(tree.nodes[node]['head']):
                        if not tree.nodes[node]['head']==0:
                            art_tree.add_edge(tree.nodes[node]['head'],node)

                mapping=dict(zip(art_tree.nodes,range(1,(len(art_tree.nodes)+1))))
                rla = nx.relabel_nodes(art_tree,mapping)
                
                abstract_root = 1000
                real_root = next(nx.topological_sort(rla))

                rla.add_node(abstract_root)
                rla.add_edge(abstract_root, real_root)
                        
                #print(rla.edges)
                #print(rla.nodes)
                
                for edgex in tree.edges:
                    dl = abs(edgex[0] - edgex[1])
                    results = open("DL-distribution.csv","a")
                    results.write(str(lang)+"\t"+str(sent_id)+"\t"+str(s_length)+"\t"+str(edgex)+"\t"+str(dl)+"\n")
                    results.close()
                    DL_lang.append(dl)
                    
                for edgey in rla.edges:
                    dl = abs(edgey[0] - edgey[1])
                    results = open("DL-distribution.csv","a")
                    results.write("Artificial"+"\t"+str(sent_id)+"\t"+str(s_length)+"\t"+str(edgex)+"\t"+str(dl)+"\n")
                    results.close()
                    DL_art.append(dl)

    print(str(lang)+"\t"+str(sum(DL_lang)/len(DL_lang))+"\t"+str(sum(DL_art)/len(DL_art)))
                    

                    
                    
            

