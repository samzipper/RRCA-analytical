## RRCA12p_02_PostprocessBaseline.py
# This will postprocess the simplified baseline model created by RRCA12p_01_...

import os
import numpy as np
import flopy
import flopy.utils.binaryfile as bf
import matplotlib.pyplot as plt
import pandas as pd

## set up your model
modelname = 'RRCA12p'
modflow_v = 'mf2k'
path2mf = 'C:/Users/samzipper/OneDrive - The University of Kansas/Research/Models/MODFLOW/mf2k.1_19/bin/mf2k.exe'  # path to MODFLOW executable
onedrive_ws = 'C:/Users/samzipper/OneDrive - The University of Kansas/Research/StreamflowDepletion/RRCA'
model_ws_simple = os.path.join(onedrive_ws, 'baseline_simple')

## Load model
mf = flopy.modflow.Modflow.load(modelname+'.nam', model_ws=model_ws_simple, 
        exe_name=path2mf, version=modflow_v, forgive=True)
nper = mf.dis.nper

### input data - discretization, hydrostratigraphy, etc
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

### output - stream leakage
# locations should be the same for all stress periods so just use first one
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
str_out = str_cbf.get_data(text='  STREAM LEAKAGE', full3D=True)
str_times = str_cbf.get_kstpkper()

# save stream locations
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
    
    # extract leakage data
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

# save strem segment output
seg_all.to_csv(os.path.join(model_ws_simple, 'RRCA12p_STR_Leakage.csv'), index=False,
           header=['SegNum','leakage','kstpkper'], float_format='%5.3e')

### input data - wells
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

well_data_all.to_csv(os.path.join(model_ws_simple, 'RRCA12p_WEL_StressPeriodData.csv'), index=False,
                     header=['lay','row','col','Qw','kstpkper'], float_format='%5.3e')

### output - constant head boundary
CHB_rows = (mf.bas6.ibound[0,:,:]==-1).nonzero()[0]
CHB_cols = (mf.bas6.ibound[0,:,:]==-1).nonzero()[1]

# open cell by cell flow data
CHB_cbf = bf.CellBudgetFile(os.path.join(model_ws_simple, 'output', modelname+'.ccf'))
CHB_times = CHB_cbf.get_kstpkper()

# for each stress period, extract for each CHB cell
start_flag = True
for sp in CHB_times:
    # load cell flow for this timestep
    CHB_sp = CHB_cbf.get_data(kstpkper=sp, text='   CONSTANT HEAD', full3D=True)[0]
    
    # convert to data frame
    CHB_df_sp = pd.DataFrame({'row': CHB_rows, 'col': CHB_cols, 
                              'leakage': CHB_sp[0, CHB_rows, CHB_cols],
                              'kstpkper': str(sp)})
    
    ## add to overall data frame
    if (start_flag):
        CHB_all = CHB_df_sp
        start_flag = False
    else:
        CHB_all = CHB_all.append(CHB_df_sp)
        
    ## status update
    print('CHB ' + str(sp) + ' complete')

CHB_cbf.close()

# save output
CHB_all.to_csv(os.path.join(model_ws_simple, 'RRCA12p_CHB_Leakage.csv'), index=False,
                     header=['row','col','leakage','kstpkper'], float_format='%5.3e')

### output - DRN
# open cell by cell flow data
DRN_cbf = bf.CellBudgetFile(os.path.join(model_ws_simple, 'output', modelname+'.dcf'))
#DRN_cbf.list_records()
DRN_times = DRN_cbf.get_kstpkper()

# for each stress period, summarize by segment
start_flag = True
for i in range(0,len(DRN_times)):
    sp = DRN_times[i]
    
    # extract active drains for this stress period
    DRN_data = pd.DataFrame(mf.drn.stress_period_data[i])
    DRN_index_summary = DRN_data.groupby(['i', 'j'], as_index=False).agg({'cond': 'sum', 'elev': 'mean'}).rename(index=str, columns={"cond": "cond_total", "elev": "elev_mean"})

    # load cell flow for this timestep
    DRN_sp = DRN_cbf.get_data(kstpkper=sp, text='          DRAINS', full3D=True)[0]
    
    # convert to data frame
    DRN_df_sp = pd.DataFrame(DRN_index_summary.copy())
    
    # extract leakage data
    DRN_df_sp['leakage'] = DRN_sp[0,DRN_index_summary['i'].tolist(),DRN_index_summary['j'].tolist()]
    DRN_df_sp['kstpkper'] = str(sp)
            
    ## add to overall data frame
    if (start_flag):
        DRN_all = DRN_df_sp
        start_flag = False
    else:
        DRN_all = DRN_all.append(DRN_df_sp)
                
    ## status update
    print('DRN ' + str(sp) + ' complete')

DRN_cbf.close()

# save output
DRN_all.to_csv(os.path.join(model_ws_simple, 'RRCA12p_DRN_Leakage.csv'), index=False,
                     header=['row','col','cond_total','elev_mean','leakage','kstpkper'], 
                     float_format='%5.3e')

### output - EVT
# open cell by cell flow data
EVT_cbf = bf.CellBudgetFile(os.path.join(model_ws_simple, 'output', modelname+'.ecf'))
#EVT_cbf.list_records()
EVT_times = EVT_cbf.get_kstpkper()

# extract locations with phreatophyte ET
EVT_matrix = mf.EVT.evtr[0][:,:]
EVT_rows = (EVT_matrix != 0).nonzero()[0]
EVT_cols = (EVT_matrix != 0).nonzero()[1]

# for each stress period, summarize by segment
start_flag = True
for sp in EVT_times:
    
    # load cell flow for this timestep
    EVT_sp = EVT_cbf.get_data(kstpkper=sp, text='              ET', full3D=True)[0]
    
    # convert to data frame
    EVT_df_sp = pd.DataFrame({'row': EVT_rows, 'col': EVT_cols, 
                              'ET': EVT_sp[EVT_rows, EVT_cols],
                              'kstpkper': str(sp)})
            
    ## add to overall data frame
    if (start_flag):
        EVT_all = EVT_df_sp
        start_flag = False
    else:
        EVT_all = EVT_all.append(EVT_df_sp)
                
    ## status update
    print('EVT ' + str(sp) + ' complete')

EVT_cbf.close()

# save output
EVT_all.to_csv(os.path.join(model_ws_simple, 'RRCA12p_EVT_Flux.csv'), index=False,
                     header=['row','col','ET','kstpkper'], 
                     float_format='%5.3e')

### output - head and water table depth
# Create the headfile object
h = bf.HeadFile(os.path.join(model_ws_simple, 'output', modelname+'.head'), text='head')

# extract data matrix
h.get_kstpkper()
head = h.get_data(kstpkper=(0, 0))  # steady state head
head[head <= mf.bas6.hnoflo] = np.nan

head_end = h.get_data(kstpkper=(1, 996))
head_end[head_end <= mf.bas6.hnoflo] = np.nan

h.close()

# head and water table depth
np.savetxt(os.path.join(model_ws_simple, 'RRCA12p_Head-SS.txt'), head[0,:,:])
np.savetxt(os.path.join(model_ws_simple, 'RRCA12p_Head-End.txt'), head_end[0,:,:])

### budget file
mfl = flopy.utils.MfListBudget(os.path.join(model_ws_simple, 'output', modelname+".out"))
df_flux, df_vol = mfl.get_dataframes()
df_flux['kstpkper'] = str_times
df_flux.to_csv(os.path.join(model_ws_simple, 'RRCA12p_BudgetFlux.csv'), index=False)

### check match between flux-specific data extractions and budget file
# STR
STR_leakage = df_flux['STREAM_LEAKAGE_IN']-df_flux['STREAM_LEAKAGE_OUT']
STR_budget = seg_all.groupby('kstpkper', as_index=False, sort=False).agg({'leakage': 'sum'})
plt.plot(STR_leakage, STR_budget['leakage'], 'o')

# CHB
CHB_leakage = df_flux['CONSTANT_HEAD_IN']-df_flux['CONSTANT_HEAD_OUT']
CHB_budget = CHB_all.groupby('kstpkper', as_index=False, sort=False).agg({'leakage': 'sum'})
plt.plot(CHB_leakage, CHB_budget['leakage'], 'o')

# DRN
DRN_leakage = df_flux['DRAINS_IN']-df_flux['DRAINS_OUT']
DRN_budget = DRN_all.groupby('kstpkper', as_index=False, sort=False).agg({'leakage': 'sum'})
plt.plot(DRN_leakage, DRN_budget['leakage'], 'o')

# EVT
EVT_leakage = df_flux['ET_IN']-df_flux['ET_OUT']
EVT_budget = EVT_all.groupby('kstpkper', as_index=False, sort=False).agg({'ET': 'sum'})
plt.plot(EVT_leakage, EVT_budget['ET'], 'o')