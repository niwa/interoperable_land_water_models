# Lookup

Lookup is a small library to map inputs to outputs.

Inputs, outputs and their relationship are described in YAML config file.

The library itself is written in C++. It comes with a BMI wrapper with a C interface.

## Configuration file

The configuration is described in a YAML file, according to the [1.2 spec](https://yaml.org/spec/1.2/spec.html).

### Inputs

Inputs are declared under the `input` node. Each input is a map describing relevant attributes. Only the `type` attribute is required.

```yaml
inputs:
  - climate: {type: str}
  - slope: {type: double, units: \"%\"}
```

Note that the `%` character has a special meaning in YAML and has to be escaped.

### Outputs

In a similar way, outputs are declared under the `output` node. Currently, only `double` outputs are supported:

```yaml
outputs:
  - TN: {type: double, units: kg_ha-1}
  - TP: {type: double, units: kg_ha-1}
```

### Mappings

Inputs values are usually clustered into a number of classes, which are then used to lookup matching outputs. The classification of input values is described in the `mappings` node. Each child node under `mappings` should correspond to one of the `inputs` nodes.

Inputs of type `str` can have their potential values mapped to classes. Inputs of type `double` are classified by mapping the lower bound of each class:

```yaml
mappings:
  # Mapping 6 possible input values to 2 classes
  climate:
    WX: warm # Warm-Extremely-Wet
    WW: warm # Warm-Wet
    WD: warm # Warm-Dry
    CX: cool # Cool-Extremely-Wet
    CW: cool # Cool-Wet
    CD: cool # Cool-Dry
  # Mapping slope value to slope classes
  slope:
  	0: low      # >=  0
    7: moderate # >=  7
    15: steep   # >= 15
```

Mappings are required for numeric inputs. If no mapping is provided for an input of type `str`, the input values are used directly to lookup matching outputs.

### Lookup

The last node in the configuration file is the `lookup` node, where outputs are mapped to input classes:

```yaml
lookup:
  warm:
    low: [1.1, 1.2]
    moderate: [2.1, 2.2]
    steep: [3.1, 3.2]
  cool:
    low: [4.1, 4.2]
    _: [5.1, 5.2]
```
The nesting order if the input classess is determined by the order in which input items appear in the `inputs` sequence.

The output sequences contain the output values in the same order as the `outputs` node. So, in the example above, 1.1 is for TN and 1.2 is for TP.

An underscore character can be used to match any class of a mapped input or any value of an unmapped input. In the example above, the `_` will match both `moderate` and `steep`.

An optional `fallback_output` key can be provided, in case the lookup does not cover all possible input combinations. If not fallback output is provided the lookup library will throw an exception for input combinations not described in the lookup node.


### Whole file example

```yaml
inputs:
  - climate: {type: str}
  - slope: {type: double, units: \"%\"}

outputs:
  - TN: {type: double, units: kg_ha-1}
  - TP: {type: double, units: kg_ha-1}

mappings:
  # Mapping 6 possible input values to 2 classes
  climate:
    WX: warm # Warm-Extremely-Wet
    WW: warm # Warm-Wet
    WD: warm # Warm-Dry
    CX: cool # Cool-Extremely-Wet
    CW: cool # Cool-Wet
    CD: cool # Cool-Dry
  # Mapping slope value to slope classes
  slope:
  	0: low      # >=  0
    7: moderate # >=  7
    15: steep   # >= 15

fallback_output: [-999, -999]

lookup:
  warm:
    low: [1.1, 1.2]
    moderate: [2.1, 2.2]
    steep: [3.1, 3.2]
  cool:
    low: [4.1, 4.2]
    _: [5.1, 5.2]
```



## Library



## BMI wrapper



# Toolchain

Lookup uses CMake as build tool.

The `builds` directory contains a number of scripts to run CMake on Linux or Windows. Copy `settings.bat` to `_settings.bat` and edit it to reflect your systems paths.

On Windows you can choose between MSVC or GCC compilers. Use `build.bat` for GCC (MinGW) and `build_msvc` for MSVC. The latter will generate a Visual Studio `*.sln` which you can then open and compile with Visual Studio.

## CMake

CMake can be downloaded from https://cmake.org/download/.

## Dependencies

The only dependency is [YAML-CPP](https://github.com/jbeder/yaml-cpp).

Download and compile it (it also uses CMake), then create a `local.cmake` next to the main `CMakeLists.txt` and set the paths to the yaml-cpp headers and compiled library in `local.cmake`. For instance like so:

```cmake
set(YAML_CPP_ROOT c:/dev/yaml-cpp)
set(YAML_CPP_INCLUDE ${YAML_CPP_ROOT}/include)
set(YAML_CPP_STATIC_LIB ${YAML_CPP_ROOT}/build/libyaml-cpp.a)
```


## MinGW

MinGW is needed to use GCC on Windows.

Download and install from [Sourceforge](https://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/installer/mingw-w64-install.exe/download). For more info on the project, visit http://mingw-w64.org.

During installation, choose the following settings:

- Architecture: x86_64
- Threads: win32
- Exception: seh

