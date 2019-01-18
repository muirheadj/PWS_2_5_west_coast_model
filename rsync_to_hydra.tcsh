#!/usr/bin/env tcsh
rsync -avuz --include='*.log' --exclude='*' muirheadj@hydra-login01.si.edu:/pool/biology/muirheadj/  ~/Documents/Post-Doc\ Maryland/epidemiology_model/logs/