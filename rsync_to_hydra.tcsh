#!/usr/bin/env tcsh
rsync -avuz --include='*.log' --exclude='*' muirheadj@hydra-login01.si.edu:/pool/biology/muirheadj/PWS_2_5_west_coast_model/logs/ \
  ~/Documents/Post-Doc_Maryland/PWS_2_5_west_coast_model/logs/
