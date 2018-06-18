import sys
import subprocess

"""Receives a id_project and house and build the data for visualization.

    Args:
        args: An array with id_project and house.

    Returns:
        Return True for success, False otherwise.

"""
def build_csv(args):
    id_project = args[1]
    house = str(args[2])
    script_result = False
    r_script = "Rscript --vanilla "
    if house.upper() == "SENADO":
        if(not subprocess.call ("{} ../../importa-dados-Senado.R {}".format(r_script, id_project), shell=True)):
            script_result = not subprocess.call ("{} ../../processa-dados-Senado.R {}".format(r_script, id_project), shell=True)
    elif house.upper() == "CAMARA":
        script_result = not subprocess.call ("{} ../../camara-process-data.R {}".format(r_script, id_project), shell=True)
    else:
        print("Wrong parameter: House just can be 'Camara' or 'Senado'")
    if(script_result):
        subprocess.call("Rscript --vanilla data-chart-{}.R {}".format(house, id_project), shell=True)
    return script_result

args = sys.argv

if len(args) >= 3:
    build_csv(args)
else:
    print("Wrong number of parameters")
    print("Usage:")
    print("./build-data.py <id_project> <house>")