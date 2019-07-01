## RRCA12p_01_Load+Simplify+RunBaseline.py
# Load RRCA12p MODFLOW model and make a new version in a new folder that will be
# the baseline model for streamflow depletion simulations.

import os
import numpy as np
import flopy
import flopy.utils.binaryfile as bf
import matplotlib.pyplot as plt
import shutil
import pandas as pd

# set up your model
modelname = 'RRCA12p'
modflow_v = 'mf2k'
path2mf = 'C:/Users/Sam/OneDrive - The University of Kansas/Research/Models/MODFLOW/mf2k.1_19/bin/mf2k.exe'  # path to MODFLOW executable
onedrive_ws = 'C:/Users/Sam/OneDrive - The University of Kansas/Research/StreamflowDepletion/RRCA'
model_ws_baseline = os.path.join(onedrive_ws, 'baseline')  # original version of model
model_ws_simple = os.path.join(onedrive_ws, 'baseline_simple')  # where to save this model

# what packages to rewrite:
packages_simplify = ['OC', 'PCG', 'WEL', 'LPF', 'DIS']

# Load model
mf = flopy.modflow.Modflow.load(modelname+'.nam', model_ws=model_ws_baseline, 
        exe_name=path2mf, version=modflow_v, forgive=True)

# inspect model
mf.get_package_list()
nper = mf.dis.nper
nstp = mf.dis.nstp[:]
perlen = mf.dis.perlen[:]/86400

# change model workspace
mf.change_model_ws(new_pth=model_ws_simple)

# set model start date
mf.dis.start_datetime = '01/01/1917'

# get rid of HYD package (not necessary)
mf.remove_package('HYD')
mf.remove_output(unit=50)

# change convergence criteria
mf.pcg.hclose = 0.01

# update OC - save at end of every stress period
oc_spd = {}
oc_spd[(0,0)] = ['save head', 'save budget']
for sp in range(1,(nper-1)):
    oc_spd[(sp,(nstp[sp]-1))] = ['save budget']
oc_spd[((nper-1),nstp[(nper-1)]-1)] = ['save head', 'save budget']
oc = flopy.modflow.ModflowOc(mf, stress_period_data=oc_spd, compact=True,
                             extension=['oc', 'head', 'ddn', 'ccf', 'ect', 'dcf', 'ibo'],
                             unitnumber=[11,30,52,40,42,43,0])

## write input files
mf.write_input(SelPackList=packages_simplify)

# copy some files from the original model
#   (if you use FloPy to write them out it will reformat to large size)
shutil.copy2(os.path.join(model_ws_baseline, modelname+'.bas'), model_ws_simple)
shutil.copy2(os.path.join(model_ws_baseline, modelname+'.dis'), model_ws_simple)
shutil.copy2(os.path.join(model_ws_baseline, modelname+'.str'), model_ws_simple)
shutil.copy2(os.path.join(model_ws_baseline, modelname+'.drn'), model_ws_simple)
shutil.copy2(os.path.join(model_ws_baseline, modelname+'.evt'), model_ws_simple)
shutil.copy2(os.path.join(model_ws_baseline, modelname+'.rch'), model_ws_simple)
if not os.path.isdir(os.path.join(model_ws_simple, 'input_data')):
    shutil.copytree(os.path.join(model_ws_baseline, 'input_data'), os.path.join(model_ws_simple, 'input_data'))

# NAM file has some unneeded data output files remaining in it; grab a template that was hand-created previously
shutil.copy2(os.path.join(model_ws_baseline, modelname+'_template.nam'), os.path.join(model_ws_simple, modelname+'.nam'))
if not os.path.isdir(os.path.join(model_ws_simple, 'output')):
    os.makedirs(os.path.join(model_ws_simple, 'output'))
success, mfoutput = mf.run_model(silent=False)
if not success:
    raise Exception('MODFLOW did not terminate normally.')