# RRCA-analytical
Comparison between Republican River Compact Administration MODFLOW model and analytical streamflow depletion models.

Structure:
src/ = code which runs models, data analysis, etc.
  RRCA12p* = files with the RRCA12p prefix are related to the RRCA12p MODFLOW model.
    RRCA12p_Load+Simplify+RunBaseline.py = this loads the RRCA12p model and makes a simplified version 
        with a smaller file size to include in the GitHub repository (reduces number of output files, etc.)
