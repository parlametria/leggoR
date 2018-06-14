import sys
import subprocess
 
# Get the arguments list 
cmdargs = str(sys.argv)
 
# Print it
if len(sys.argv) == 3:
    id_project = sys.argv[1]
    house = str(sys.argv[2])
    if house.upper() == "SENADO":
        if(not subprocess.call ("Rscript --vanilla ../../importa-dados-Senado.R {}".format(id_project), shell=True)):
            if(not subprocess.call ("Rscript --vanilla ../../processa-dados-Senado.R {}".format(id_project), shell=True)):
                subprocess.call("Rscript --vanilla ../../build-chart.R {} {}".format(id_project, house), shell=True)
            print("ca")
    elif house.upper() == "CAMARA":
        print("CAMARA")
    else:
        print("Wrong parameter: House just can be 'Camara' or 'Senado'")
else:
    print("Wrong number of parameters")
    print("Usage:")
    print("./build-data.py <id_project> <house>")