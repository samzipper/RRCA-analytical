## RRCA12p_Load+Simplify+RunBaseline.py

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
model_ws_baseline = 'C:/Users/Sam/OneDrive - The University of Kansas/Research/StreamflowDepletion/RRCA/baseline'  # original version of model
model_ws_simple = 'modflow/baseline_simple'  # where to save this model

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
                             extension=['oc', 'head', 'ddn', 'ccf', 'ibo'],
                             unitnumber=[11,30,52,40,0])

# turn off LPF cell-by-cell flow
mf.lpf.ipakcb = 0

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

## save some files as grids
mf.lpf.plot()  # save hk, sy and ss
mf.dis.plot()  # save top and botm
mf.bas6.plot() # save ibound and strt

np.savetxt(os.path.join(model_ws_simple, modelname+'_LPF-hk.txt'),
           mf.lpf.hk[0,:,:], fmt='%e', delimiter=' ')
np.savetxt(os.path.join(model_ws_simple, modelname+'_LPF-ss.txt'),
           mf.lpf.ss[0,:,:], fmt='%5.2e', delimiter=' ')
np.savetxt(os.path.join(model_ws_simple, modelname+'_LPF-sy.txt'),
           mf.lpf.sy[0,:,:], fmt='%5.2e', delimiter=' ')

np.savetxt(os.path.join(model_ws_simple, modelname+'_DIS-top.txt'),
           mf.dis.top[:,:], fmt='%10.6f', delimiter=' ')
np.savetxt(os.path.join(model_ws_simple, modelname+'_DIS-botm.txt'),
           mf.dis.botm[0,:,:], fmt='%6.2f', delimiter=' ')

np.savetxt(os.path.join(model_ws_simple, modelname+'_BAS6-ibound.txt'),
           mf.bas6.ibound[0,:,:], fmt='%1d', delimiter=' ')
np.savetxt(os.path.join(model_ws_simple, modelname+'_BAS6-strt.txt'),
           mf.bas6.strt[0,:,:], fmt='%10.6f', delimiter=' ')

np.savetxt(os.path.join(model_ws_simple, modelname+'_RCH-rech.txt'),
           mf.rch.rech[0][:,:], fmt='%5.2e', delimiter=' ')

# getbudget files
mfl = flopy.utils.MfListBudget(os.path.join(model_ws_simple, 'output', modelname+".out"))
df_flux, df_vol = mfl.get_dataframes()

## set up stream reach data for extracting results
# should be the same for all stress periods so just use first one
stream_data = pd.DataFrame(mf.str.stress_period_data[0])

# some cells have multiple stream segments; we will distribute based on the proportion of total conductance
stream_index_summary = stream_data.groupby(['k', 'i', 'j'], as_index=False).agg({'cond': 'sum'}).rename(index=str, columns={"cond": "cond_total"})
stream_data = pd.merge(stream_data, stream_index_summary,
                    how='left', on=['k','i', 'j'])

lay = stream_data['k'].tolist()
row = stream_data['i'].tolist()
col = stream_data['j'].tolist()

# open cell by cell flow data
str_cbf = bf.CellBudgetFile(os.path.join(model_ws_simple, 'output', modelname+'.scf'))
str_cbf.list_records()
str_out = str_cbf.get_data(text='  STREAM LEAKAGE', full3D=True)
str_times = str_cbf.get_kstpkper()

## save flux output
df_flux['kstpkper'] = str_times
df_flux.to_csv(os.path.join(model_ws_simple, 'RRCA12p_BudgetFlux.csv'), index=False)

## save stream locations
stream_df_sp1 = stream_data.copy()
stream_df_sp1['leakage'] = str_out[0][lay,row,col]
stream_df_sp1.to_csv(os.path.join(model_ws_simple, 'RRCA12p_STR_StressPeriod1.csv'), index=False,
           header=['lay','row','col','SegNum','ReachNum','flow','stage','cond','sbot','stop','width','slope','rough','cond_total','leakage'])

# for each stress period, summarize by segment
start_flag = True
for sp in str_times:
    # load cell flow for this timestep
    str_sp = str_cbf.get_data(kstpkper=sp, text='  STREAM LEAKAGE', full3D=True)[0]
    
    # convert to data frame
    stream_df_sp = pd.DataFrame(stream_data.copy())
    
    # eztract leakage data
    #stream_df_sp['leakage'] = str_sp[stream_df_sp['k'],stream_df_sp['i'],stream_df_sp['j']]
    stream_df_sp['leakage'] = str_sp[lay,row,col]*(stream_df_sp['cond']/stream_df_sp['cond_total'])
    
    # summarize by segment
    seg_sp = stream_df_sp.groupby('segment', as_index=False).agg({'leakage': 'sum'})
    seg_sp['kstpkper'] = str(sp)
            
    ## add to overall data frame
    if (start_flag):
        seg_all = seg_sp
        start_flag = False
    else:
        seg_all = seg_all.append(seg_sp)
                
    ## status update
    print(str(sp) + ' complete')

str_cbf.close()

## check to make sure STR output matches budget in LIST file
#net_leakage = df_flux['STREAM_LEAKAGE_IN']-df_flux['STREAM_LEAKAGE_OUT']
#seg_compare = seg_all.groupby('kstpkper', as_index=False, sort=False).agg({'leakage': 'sum'})
#plt.plot(net_leakage, seg_compare['leakage'], 'o')
#plt.plot([min(net_leakage), max(net_leakage)], [min(net_leakage), max(net_leakage)])

# save strem segment utput
seg_all.to_csv(os.path.join(model_ws_simple, 'RRCA12p_STR_Leakage.csv'), index=False,
           header=['SegNum','leakage','kstpkper'])

## extract boundary condition data that can be plotted in R
# wells
well_data = mf.wel.stress_period_data
start_flag_w = True
for sp in range(0,nper):
    # get data for this stress peroid
    well_data_sp = pd.DataFrame(well_data[sp])
    
    if ('flux' in well_data_sp.columns):
        well_data_sp['kstpkper'] = [str_times[sp]]*len(well_data_sp)
        
        ## add to overall data frame
        if (start_flag_w):
            well_data_all = well_data_sp
            start_flag_w = False
        else:
            well_data_all = well_data_all.append(well_data_sp)

# save strem segment utput
well_data_all.to_csv(os.path.join(model_ws_simple, 'RRCA12p_WEL_StressPeriodData.csv'), index=False,
                     header=['lay','row','col','Qw','kstpkper'], float_format='%5.3f')

## get head
## look at head output
# Create the headfile object
h = bf.HeadFile(os.path.join(model_ws_simple, 'output', modelname+'.head'), text='head')

# extract data matrix
h.get_kstpkper()
head = h.get_data(kstpkper=(0, 0))  # steady state head
head[head <= mf.bas6.hnoflo] = np.nan
plt.imshow(head[0,:,:]); plt.colorbar()

# calculate WTD
wtd = mf.dis.top[:,:] - head[0,:,:]

head_end = h.get_data(kstpkper=(1, 996))
head_end[head_end <= mf.bas6.hnoflo] = np.nan

ddn = head[0,:,:] - head_end[0,:,:]

## save data to plot in R
# head and water table depth
np.savetxt(os.path.join(model_ws_simple, 'RRCA12p_Head-SS.txt'), head[0,:,:])
np.savetxt(os.path.join(model_ws_simple, 'RRCA12p_Head-End.txt'), head_end[0,:,:])
np.savetxt(os.path.join(model_ws_simple, 'RRCA12p-WTD.txt'), wtd)