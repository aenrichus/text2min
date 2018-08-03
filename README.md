# text2min
This function takes in a list or column of free text and converts that text into integer minutes.

## Version History

* v1.0 - Initial Release

## Planned Upgrades

* fix the Known Issues below
* add a parameter that allows for selection of minutes or hours
* convert this into an R package for distribution

## Known Issues

* preceding characters (~10 minutes) fail entirely
* written fractions (2 1/2 hours) use only the initial integer
* ranges (1.5-2 hours) use only the initial number
* need to include "an" and "a" in the dictionary file
