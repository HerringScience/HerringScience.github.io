There are two function scripts, one for BiomassCalcs and one for Echoview Integration

These are **not directly run** but sourced/called in the two survey-specific scripts



Some pathways are directed to HSC GitHub regardless of users (this is good)

Some pathways are directed to Darren's local drive and would need to change per user (this is bad)



**Integration Rscript\_YYYY\_MM\_DD\_TS.R**

\-replaces all HSC vbscripts for changing Echoview settings, sorting data, and exporting Region/Map

\-if this works equivalently this is a solid time saved and reduces our workflow (5 vbscripts with manual tweaks and Excel work vs. 1 R script)



> "vessel\_search" right/left will need to change based on users pathway (e.g. '62' works for me but not others, tweak until vessel\_search points towards 2-letter vessel name)



**BiomassCalcs\_YYYY\_MM\_DD\_TS.R**

\-Integrations need to be separated by GB/SI in same folder (test with GB1)



