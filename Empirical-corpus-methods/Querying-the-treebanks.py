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
#for i in ud_files:                                       # reads file of each language one by one
