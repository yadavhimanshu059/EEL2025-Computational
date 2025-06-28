import os
from io import open
from conllu import parse
import networkx as nx
from operator import itemgetter

directory = "./SUD"                   # directory containing the UD scheme tree files in CONLLU format
sud_files = []
for root, dirs, files in os.walk(directory):
    for file in files:
        if file.endswith('train.conllu'):
            fullpath = os.path.join(root, file)
            sud_files.append(fullpath)            # creates a list of path of all files (file of each language) from the directory

print(sud_files)

for i in sud_files:                  # reads file of each language one by one
    lang = str(i).replace("./SUD", "")
    lang=lang.replace("-sud-train.conllu", "")            # lang variable stores the language code
    lang=lang.replace("-sud-test.conllu", "")
    data_file = open(str(i),'r',encoding='utf-8').read()
    sentences = []
    sentences = parse(data_file)                         # parses the CONLLU format
    print(lang)
    sent_id =0
    results = open("Verb_rate.csv","w")
    results.write("Sent_id,S_length,Verb_count\n")
    results.close()
    for sentence in sentences[0:]:
        sent_id+=1
        print(sent_id)
        if sent_id<5000:
            tree =   nx.DiGraph()                              # An empty directed graph (i.e., edges are uni-directional)
            for nodeinfo in sentence[0:]:                    # retrieves information of each node from dependency tree in UD format
                entry=list(nodeinfo.items())
                if not entry[7][1]=='punct':
                    tree.add_node(entry[0][1], form=entry[1][1], lemma=entry[2][1], upostag=entry[3][1], xpostag=entry[4][1], feats=entry[5][1], head=entry[6][1], deprel=entry[7][1], deps=entry[8][1], misc=entry[9][1])                #adds node to the directed graph
            ROOT=0
            tree.add_node(ROOT)                            # adds an abstract root node to the directed graph

            for nodex in tree.nodes:
                if not nodex==0:
                    if tree.has_node(tree.nodes[nodex]['head']):                                         # to handle disjoint trees
                        tree.add_edge(tree.nodes[nodex]['head'],nodex,drel=tree.nodes[nodex]['deprel'])       # adds edges as relation between nodes

            #print(tree.edges)

            s_length = 0
            verb_count = 0
            core = []
            for nodex in tree.nodes:
                s_length += 1
                if nodex!=0:
                    if tree.nodes[nodex]['upostag']=="VERB":
                        core.append("VERB")
                        verb_count += 1
                    if tree.nodes[nodex]['upostag']=="NOUN" and tree.nodes[nodex]['deprel']=="subj" and (tree.nodes[tree.nodes[nodex]['head']]['upostag'] in ["VERB","AUX"]):
                        core.append("SUBJ")
                    if tree.nodes[nodex]['upostag']=="NOUN" and tree.nodes[nodex]['deprel']=="comp:obj" and (tree.nodes[tree.nodes[nodex]['head']]['upostag'] in ["VERB","AUX"]):
                        core.append("dOBJ")
                    if tree.nodes[nodex]['upostag']=="NOUN" and tree.nodes[nodex]['deprel']=="comp:obl" and (tree.nodes[tree.nodes[nodex]['head']]['upostag'] in ["VERB","AUX"]):
                        core.append("iOBJ")
            print(core)
                        
                        

            results = open("Verb_rate.csv","a")
            results.write(str(sent_id)+","+str(s_length)+","+str(verb_count)+"\n")
            results.close()
                    
                    
            

