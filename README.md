# RRCA-analytical
Comparison between Republican River Compact Administration MODFLOW model and analytical streamflow depletion models.

## Table of Contents

 - modflow/ = MODFLOW input and output files for various models
    - baseline_simple/ = simplified baseline model from RRCA
 - src/ = code which runs models, data analysis, etc.
    - RRCA12p* = files with the RRCA12p prefix are related to the RRCA12p MODFLOW model.
        - RRCA12p_01_Load+Simplify+RunBaseline.py = this loads the RRCA12p model and makes a simplified version 
            with a smaller file size to include in the GitHub repository (reduces number of output files, etc.).
            The modflow is run and stored in modflow/baseline_simple and some simple output files are generated.
        - RRCA12p_02_PlotBCs+SelectWellSample.R = inspect the RRCA12p model and select a subsample of wells 
            that will be turned on/off for analytical depletion function testing.
        
       
