# Typologies

A typology is defined as combination of certain attributes, such as slope, climate, wetness, etc. which together impact N & P losses from different land uses.

The goal of the TypoloyLookup is to expose annual N and P losses for every typology. The current implementation uses dummy data for now, as a proof of concept.

| Typology   | Nitrogen [kg_ha-1] | Phosphorus [kg_ha-1] |
| ---------- | ------------------ | -------------------- |
| Typology_1 | 1.1                | 1.2                  |
| Typology_2 | 2.1                | 2.2                  |
| Typology_3 | 3.1                | 3.2                  |

## Contents

1. typo_lib: a shared library with core functionality
2. typo_bmi_lib: a BMI-wrapped version of typo_lib
3. typo_app.exe: small test program using typo_lib
4. typo_bmi_app: small test program using typo_bmi_lib

## BMI Lookups

BMI is not particularly suited to share lookup tables between components.

The current TypologyLookup offers two methods to share it's data.

### Method 1: Single value queries

With the first method, the consuming component first tells the lookup component for which typology it wants to retrieve values.

```c++
SetValue("typology", "Typology_1");
```

The lookup component exposes an output variable for each column of it's data table:

```c++
GetValue("nitrogen", (char*)&n_value);
GetValue("phosphorus", (char*)&p_value);
```

### Method 2: Whole table requests

A special output variable called `table` allows a consumer to request the full data table:

```c++
GetValue("table", (char*)&table);
```

To process the table, the consumer will need to know it's dimensions. While the full size can be retrieved using `GetVarSize("table", &size);`, the current implementation does not yet expose the number of rows (i.e. typologies) or columns (i.e. substances). 

## ToDo's

- [ ] Choose units (kg_ha-1 or BMI's preferred g_m2)
- [ ] Load data from file during initialization
- [ ] Expose row and column size and labels
- [ ] Spatial queries (see below)
- [ ] Tests

## Spatial queries

The current implementation requires the consumer to have a polygon layer with a typology assigned to each polygon.

An alternative would be to have the lookup component return N & P not for a given typology, but for a polygon. The consumer then doesn't need to be aware of the typology layer and can simply request values for the area covered by a sub-catchment, for instance. The lookup component will itself compute the intersection between the provided polygon and it's typology layer.



