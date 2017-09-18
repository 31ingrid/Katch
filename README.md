The .tpl file Katch.tpl can be compiled by ADMB by the command:
admb Katch.tpl.

Navigate to ./src
Code to run the BSAI Arrowtooth flounder assessment model can be run as follows:
./Katch -ind atf_bsai.dat

Code to run the GOA Arrowtooth flounder assessment model can be run as follows:
./Katch -ind atf_goa.dat

Code to run the BSAI Kamchatka flounder model can be run as follows:
./Katch -ind kam.dat

Output is called Katch.rep (regardless of the input .dat file).
Read in the read-rep.R file and then use the Katch_plotting.R file to 
make plots.


