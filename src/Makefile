# Choix du compilateur
FC=gfortran

FLAGS=-DG95=1 -g  -ffpe-trap=overflow,invalid,zero,underflow -Wconversion -fbounds-check

#definition de l'executable
#EXECDIR = ../bin/
#EXECDIR = ../../compensation_gravi/Compensation_2016_2017_sans_gradient/
EXEC = mcgravi

all : comp link

comp : Port_routines.o util_str.o sys_utils.o str_const.o raw_data.o param.o lect_param.o lect_rawdata.o mc_data.o write_matr.o rempli_matMC.o sparse_cholesky.o mc_inversion.o mc_simulation.o ecriture.o gmt.o write_Html.o write_xml.o mc_statistic.o BLKFCLT.o mcgravi.o mc_gravi.o
	
link : 
	$(FC) -static -o $(EXECDIR)$(EXEC) mc_gravi.o Port_routines.o util_str.o sys_utils.o str_const.o raw_data.o param.o lect_param.o lect_rawdata.o mc_data.o write_matr.o rempli_matMC.o sparse_cholesky.o mc_inversion.o mc_simulation.o ecriture.o gmt.o write_Html.o write_xml.o mc_statistic.o BLKFCLT.o mcgravi.o 

install:
	cp $(EXECDIR)$(EXEC) /usr/local/bin
	chmod a+x /usr/local/bin/$(EXEC)

Port_routines.o : Port_routines_gfortran.f90
	$(FC) -o Port_routines.o -c  $(FLAGS) Port_routines_gfortran.f90	

util_str.o : util_str.f90
	$(FC) $(FLAGS) -c util_str.f90
	
sys_utils.o : sys_utils.f90
	$(FC) $(FLAGS) -c sys_utils.f90
	
str_const.o : str_const.f90
	$(FC) -c $(FLAGS) str_const.f90
	
raw_data.o : raw_data.f90
	$(FC) -c $(FLAGS) raw_data.f90
	
param.o : param.f90
	$(FC) -c $(FLAGS) param.f90
	
lect_param.o : lect_param.f90
	$(FC) -c $(FLAGS) lect_param.f90
	
lect_rawdata.o : lect_rawdata.f90
	$(FC) -c $(FLAGS) lect_rawdata.f90
	
mc_data.o : mc_data.f90
	$(FC) -c $(FLAGS) mc_data.f90
	
write_matr.o : write_matr.f90
	$(FC) -c $(FLAGS) write_matr.f90
	
rempli_matMC.o : rempli_matMC.f90
	$(FC) -c $(FLAGS) rempli_matMC.f90

sparse_cholesky.o : sparse_cholesky.f90
	$(FC) -c $(FLAGS) sparse_cholesky.f90
	
mc_inversion.o : mc_inversion.f90
	$(FC) -c $(FLAGS) mc_inversion.f90
	
mc_simulation.o : mc_simulation.f90
	$(FC) -c $(FLAGS) mc_simulation.f90
	
ecriture.o : ecriture.f90
	$(FC) -c $(FLAGS) ecriture.f90
	
gmt.o : gmt.f90
	$(FC) -c $(FLAGS) gmt.f90
	
write_Html.o : write_Html.f90
	$(FC) -c $(FLAGS) write_Html.f90
	
write_xml.o : write_xml.f90
	$(FC) -c $(FLAGS) write_xml.f90
	
mc_statistic.o : mc_statistic.f90
	$(FC) -c $(FLAGS) mc_statistic.f90

BLKFCLT.o : BLKFCLT.F
	$(FC) -c $(FLAGS) BLKFCLT.F
		
mcgravi.o : mcgravi.f90
	$(FC) -c $(FLAGS) mcgravi.f90
	
mc_gravi.o : mc_gravi.f90
	$(FC) -c $(FLAGS) mc_gravi.f90


			
clean :
	rm -f  *.o 
	rm -f *.mod
	rm -f  *.obj
	rm -f *~
