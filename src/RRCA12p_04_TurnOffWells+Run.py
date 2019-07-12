## RRCA12p_04_TurnOffWells+Run.py
# This script is intended to loop through all the wells that will be turned off,
# run the MODFLOW model, and postprocess/save the output.

import os
import numpy as np
import flopy
import flopy.utils.binaryfile as bf
import shutil
import pandas as pd
import datetime

## set up your model
modelname = 'RRCA12p'
modflow_v = 'mf2k'
path2mf = 'C:/Users/gsas/OneDrive - The University of Kansas/Research/Models/MODFLOW/mf2k.1_19/bin/mf2k.exe'  # path to MODFLOW executable
onedrive_ws = 'C:/Users/gsas/OneDrive - The University of Kansas/Research/StreamflowDepletion/RRCA'

## paths for baseline model
model_ws_simple = os.path.join(onedrive_ws, 'baseline_simple')
output_path = os.path.join('modflow', 'baseline_simple')

## Load model
mf = flopy.modflow.Modflow.load(modelname+'.nam', model_ws=model_ws_simple, 
        exe_name=path2mf, version=modflow_v, forgive=True)
nper = mf.dis.nper

## change model workspace
output_well = os.path.join('modflow', 'wells')
model_ws_well = os.path.join(onedrive_ws, 'wells')
mf.change_model_ws(new_pth = model_ws_well)

# copy input data which is referred to within model files
if not os.path.isdir(os.path.join(model_ws_well, 'input_data')):
    shutil.copytree(os.path.join(model_ws_simple, 'input_data'), os.path.join(model_ws_well, 'input_data'))
    
# make output folder
if not os.path.isdir(os.path.join(model_ws_well, 'output')):
    os.makedirs(os.path.join(model_ws_well, 'output'))

## extract some data that will be used for postprocessing and won't change between wells
# stream locations
stream_data = pd.DataFrame(mf.str.stress_period_data[0])
stream_index_summary = stream_data.groupby(['k', 'i', 'j'], as_index=False).agg({'cond': 'sum'}).rename(index=str, columns={"cond": "cond_total"})
stream_data = pd.merge(stream_data, stream_index_summary,
                       how='left', on=['k','i', 'j']) 
lay = stream_data['k'].tolist()
row = stream_data['i'].tolist()
col = stream_data['j'].tolist()

# CHB locations
CHB_rows = (mf.bas6.ibound[0,:,:]==-1).nonzero()[0]
CHB_cols = (mf.bas6.ibound[0,:,:]==-1).nonzero()[1]

# baseline WEL input
baseline = flopy.modflow.Modflow.load(modelname+'.nam', model_ws=model_ws_simple,
                                      exe_name=path2mf, version=modflow_v, forgive=True,
                                      load_only='wel')
wel_baseline = baseline.wel

## load CSV file with well sample to turn off (produced by script RRCA12p_03_)
wells_all = pd.read_csv(os.path.join('results', 'RRCA12p_03_WellSample.csv'))
wells_sample = wells_all[wells_all['sample_lhs']]
wells_sample = wells_sample.reset_index(drop=True)
n_wells = len(wells_sample['WellNum'])

## loop through wells
for w in range(0, n_wells):
    # extract well info
    WellNum = wells_sample['WellNum'][w]
    well_row = wells_sample['row'][w] - 1  # need to subtract 1 for python indexing
    well_col = wells_sample['col'][w] - 1
    
    ## change well stress period data to 0 for all timesteps
    for sp in range(1,nper):  # start at 1 because no pumping in sp 0 (steady-state)
        # figure out if this well is active in the current stress period
        i = np.where(mf.wel.stress_period_data[sp]['i'] == well_row)
        j = np.where(mf.wel.stress_period_data[sp]['j'] == well_col)
        well_index = np.intersect1d(i, j)
        
        # if it is active, disable it
        if well_index.size == 0:
            pass
            #print('Well '+str(WellNum)+' inactive for sp '+str(sp))
        elif well_index.size == 1:
            mf.wel.stress_period_data[sp]['flux'][well_index] = [0.0]
            #print('Well '+str(WellNum)+' disabled for sp '+str(sp))
        else:
            raise ValueError('Well '+str(WellNum)+' index length error for sp '+str(sp))
    
    try:
        ## set up model
        mf.write_input(SelPackList=['WEL']) # write WEL file
        
        # copy template NAM file that refers back to baseline_simple for all other inputs
        shutil.copy2(os.path.join(model_ws_simple, modelname+'_well_template.nam'), os.path.join(model_ws_well, modelname+'.nam'))
        
        ## run model
        success, mfoutput = mf.run_model(silent=True)
        if not success:
            raise Exception('MODFLOW did not terminate normally.')
    except:
        print('Well '+str(WellNum)+' model run error')
    
    ### postprocessing
    ## output - stream leakage
    try:
        # open cell by cell flow data
        str_cbf = bf.CellBudgetFile(os.path.join(model_ws_well, 'output', modelname+'.scf'))
        str_out = str_cbf.get_data(text='  STREAM LEAKAGE', full3D=True)
        str_times = str_cbf.get_kstpkper()
        
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
                    
            # add to overall data frame
            if (start_flag):
                seg_all = seg_sp
                start_flag = False
            else:
                seg_all = seg_all.append(seg_sp)
        
        str_cbf.close()
        
        # save stream segment output
        seg_all.to_csv(os.path.join(model_ws_well, 'RRCA12p_Well'+str(WellNum)+'_STR_Leakage.csv'), index=False,
                       header=['SegNum','leakage','kstpkper'], float_format='%5.3e')
    except:
        print('Well '+str(WellNum)+' STR postprocessing error')

    
    ## output - constant head boundary
    try:
        # open cell by cell flow data
        CHB_cbf = bf.CellBudgetFile(os.path.join(model_ws_well, 'output2', modelname+'.ccf'))
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
            
            # add to overall data frame
            if (start_flag):
                CHB_all = CHB_df_sp
                start_flag = False
            else:
                CHB_all = CHB_all.append(CHB_df_sp)
        
        CHB_cbf.close()
        
        # save output
        CHB_all.to_csv(os.path.join(model_ws_well, 'RRCA12p_Well'+str(WellNum)+'_CHB_Leakage.csv'), index=False,
                       header=['row','col','leakage','kstpkper'], float_format='%5.3e')
    except:
        print('Well '+str(WellNum)+' CHB postprocessing error')
    
    ## output - DRN
    try:
        # open cell by cell flow data
        DRN_cbf = bf.CellBudgetFile(os.path.join(model_ws_well, 'output', modelname+'.dcf'))
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
                    
            # add to overall data frame
            if (start_flag):
                DRN_all = DRN_df_sp
                start_flag = False
            else:
                DRN_all = DRN_all.append(DRN_df_sp)
        
        DRN_cbf.close()
        
        # save output
        DRN_all.to_csv(os.path.join(model_ws_well, 'RRCA12p_Well'+str(WellNum)+'_DRN_Leakage.csv'), index=False,
                       header=['row','col','cond_total','elev_mean','leakage','kstpkper'], float_format='%5.3e')
    except:
        print('Well '+str(WellNum)+' DRN postprocessing error')
    
    ## budget file
    try:
        mfl = flopy.utils.MfListBudget(os.path.join(model_ws_well, 'output', modelname+".out"))
        df_flux, df_vol = mfl.get_dataframes()
        df_flux['kstpkper'] = str_times
        df_flux.to_csv(os.path.join(model_ws_well, 'RRCA12p_Well'+str(WellNum)+'_BudgetFlux.csv'), index=False)
    except:
        print('Well '+str(WellNum)+' LST postprocessing error')
    
    ### reload baseline WEL file 
    mf.remove_package('WEL')  # remove modified well
    mf.add_package(wel_baseline)
    
    ## status update
    print('Well '+str(w)+' of '+str(n_wells)+' complete, '+str(datetime.datetime.now()))