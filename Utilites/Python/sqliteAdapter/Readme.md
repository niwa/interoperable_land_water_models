# SqliteAdapter

Takes variables from one sqlite table column and puts them into a column in another table, which could be in a different database and could have a different name.

Each table needs an index column (primary Key).
Columns can be renamed.

The mapping is defined in a yaml configuration file.

## SqliteAdapter.py
The data copying is done by this Python script. It runs in Python 3.6, and probably most other Python 3.x versions. It depends on PyYaml to read the configuration file, and sqlite3 (a standard Python library) for the database operations.

## bmi_sqliteAdapter.cpp
Wraps the Python script to make it comply with OE BMI enough to run in DIMR and bmi-runner. It depends on Pybind11.
