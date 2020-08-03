# Iron Python program to run loosely coupled system of BMI-compliant models

import sys
import clr
import System

sessionLogger = lambda a,b: sys.stdout.write("{}: {}\n".format(a, b))

myListOfModels = [
    ("bmi_iterator.dll", "lookup-iterator.yaml"),
    ("bmi_aggregator.dll", "lookup-aggregator.yaml")]

DeltaShellHome = r"C:\Models\Programs\DeltaShell\1.3.0.0\DeltaShell"
# sessionLogger = lambda a,b: sys.stdout.write("{}: {}\n".format(a, b))

sys.path.append(DeltaShellHome)
clr.AddReference("BasicModelInterface.dll")
from BasicModelInterface import BasicModelInterfaceLibrary as BmiLib

def bmiLibLoadRunFinish(bmi_lib, bmi_config):
    wrappedLib = BmiLib(bmi_lib)
    wrappedLib.Logger = lambda a,b: sys.stdout.write("{}: {}\n".format(a, b))
    wrappedLib.Initialize(bmi_config)
    wrappedLib.Update(1)
    wrappedLib.Finish()

for modelConfig in myListOfModels:
    bmiLibLoadRunFinish(modelConfig[0], modelConfig[1])
