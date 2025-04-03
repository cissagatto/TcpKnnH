##############################################################################
# HYBRID PARTITIONS FOR MULTI-LABEL CLASSIFICATION DEFAULT VERSION           #
# Copyright (C) 2025                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# 1 - Prof Elaine Cecilia Gatto                                              #
# 2 - Prof PhD Ricardo Cerri                                                 #
# 3 - Prof PhD Mauri Ferrandin                                               #
# 4 - Prof PhD Celine Vens                                                   #
# 5 - PhD Felipe Nakano Kenji                                                #
# 6 - Prof PhD Jesse Read                                                    #
#                                                                            #
# 1 = Federal University of São Carlos - UFSCar - https://www2.ufscar.br     #
# Campus São Carlos | Computer Department - DC - https://site.dc.ufscar.br | #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br | Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       # 
#                                                                            # 
# 1 = Federal University of Lavras - UFLA                                    #
#                                                                            # 
# 2 = State University of São Paulo - USP                                    #
#                                                                            # 
# 3 - Federal University of Santa Catarina Campus Blumenau - UFSC            #
# https://ufsc.br/                                                           #
#                                                                            #
# 4 and 5 - Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium     #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
# 6 - Ecole Polytechnique | Institut Polytechnique de Paris | 1 rue Honoré   #
# d’Estienne d’Orves - 91120 - Palaiseau - FRANCE                            #
#                                                                            #
##############################################################################


import sys
import os
import io
from io import BytesIO

sys.path.append(os.getcwd()) 

import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier  
import joblib
import pickle


if __name__ == '__main__':   
  
    print("SINGLE-LABEL")

    # obtendo argumentos da linha de comando
    train = pd.read_csv(sys.argv[1]) # conjunto de treino
    # valid = pd.read_csv(sys.argv[2]) # conjunto de validação
    test = pd.read_csv(sys.argv[2])  # conjunto de teste
    start = int(sys.argv[3])         # inicio do espaço de rótulos    
    directory = sys.argv[4]          # diretório para salvar as predições 
    
    # num_labels = 1
    # train = pd.read_csv("/dev/shm/stand-GpositiveGO/Tested/Split-6/Group-2/GpositiveGO-split-tr-6-group-2.csv")
    # valid = pd.read_csv("/dev/shm/stand-GpositiveGO/Tested/Split-6/Group-2/GpositiveGO-split-vl-6-group-2.csv")
    # test = pd.read_csv("/dev/shm/stand-GpositiveGO/Tested/Split-6/Group-2/GpositiveGO-split-ts-6-group-2.csv")
    # start = 912
    # directory = "/dev/shm/stand-GpositiveGO/Tested/Split-6/Group-2"
    
    # juntando treino com validação
    #train = pd.concat([train,valid],axis=0).reset_index(drop=True)
    
    # treino: separando os atributos e os rótulos
    X_train = train.iloc[:, :start]    # atributos 
    Y_train  = train.iloc[:, start:] # rótulos 
    
    # teste: separando os atributos e os rótulos
    X_test = test.iloc[:, :start]     # atributos
    Y_test = test.iloc[:, start:] # rótulos verdadeiros
    
    # obtendo os nomes dos rótulos
    labels_y_train = list(Y_train.columns)
    labels_y_test = list(Y_test.columns)
    
    # obtendo os nomes dos atributos
    attr_x_train = list(X_train.columns)
    attr_x_test = list(X_test.columns)
    
    # parametros do classificador base
    random_state = 0    
    n_estimators = 200
    
    # inicializa o classificador base
    rf = RandomForestClassifier(n_estimators = n_estimators, 
    random_state = random_state)
    
    # treino
    y = np.squeeze(Y_train.values)
    # rf.fit(X_train.values, y)
    # rf.fit(X_train, y)
    # treino sem .values para manter os nomes das colunas
    rf.fit(X_train, np.squeeze(Y_train.values))

    # predições binárias
    y_pred_bin = pd.DataFrame(rf.predict(X_test))
    y_pred_bin.columns = labels_y_test

    # predições probabilísticas
    probabilities = rf.predict_proba(X_test)
    probabilities_2 = pd.DataFrame(probabilities)
    probabilities_2.columns = [f'prob_0', f'prob_1']
    
    # setando nome do diretorio e arquivo para salvar
    true = (directory + "/y_true.csv")     
    pred = (directory + "/y_pred_bin.csv") 
    proba = (directory + "/y_pred_proba.csv")  
    
    #  salvando true labels and predict labels
    y_pred_bin.to_csv(pred, index=False)
    Y_test.to_csv(true, index=False)
    probabilities_2.to_csv(proba, index=False)
    
    #print("")
    
    # Lista para armazenar os tamanhos dos modelos
    model_sizes = []

    # Obtendo tamanho do modelo com pickle (sem salvar no disco)
    model_bytes_pickle = pickle.dumps(rf)
    file_size_bytes_pickle = len(model_bytes_pickle)
    file_size_kb_pickle = file_size_bytes_pickle / 1024  
    file_size_mb_pickle = file_size_kb_pickle / 1024 
    file_size_gb_pickle = file_size_mb_pickle / 1024  

    model_sizes.append(["pickle", file_size_bytes_pickle, file_size_kb_pickle, file_size_mb_pickle,file_size_gb_pickle])
    #print(f"Pickle model size: {file_size_gb_pickle:.2f} GB")

    # Obtendo tamanho do modelo com joblib (sem salvar no disco)
    buffer = BytesIO()
    joblib.dump(rf, buffer)
    file_size_bytes_joblib = buffer.getbuffer().nbytes
    file_size_kb_joblib = file_size_bytes_joblib / 1024  
    file_size_mb_joblib = file_size_kb_joblib / 1024  
    file_size_gb_joblib = file_size_mb_joblib / 1024  

    model_sizes.append(["joblib", file_size_bytes_joblib, file_size_kb_joblib, file_size_mb_joblib,file_size_gb_joblib])
    #print(f"Joblib model size: {file_size_gb_joblib:.2f} GB")
    #print("")

    # Criando DataFrame para salvar os tamanhos dos modelos
    df_size = pd.DataFrame(model_sizes, columns=["Format", "Size (Bytes)", "Size (KB)", "Size (MB)","Size (GB)"])

    # Salvando os tamanhos dos modelos em um CSV
    csv_path = os.path.join(directory, "model-sizes.csv")
    df_size.to_csv(csv_path, index=False)

    print(f"Model sizes saved to '{csv_path}'")
    print("")
