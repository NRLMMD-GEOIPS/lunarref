    # # # This source code is subject to the license referenced at
    # # # https://github.com/NRLMMD-GEOIPS.

Lunar Reflectance GeoIPS Plugin
===============================

The lunarref package is a GeoIPS-compatible plugin, intended to be used within
the GeoIPS ecosystem.  Please see the
[GeoIPS Documentation](https://github.com/NRLMMD-GEOIPS/geoips#readme) for
more information on the GeoIPS plugin architecture and base infrastructure.

Package Overview
-----------------

The Lunar Reflectance package performs lunar corrections based on the lunar
zenith angle.

System Requirements
---------------------

* geoips >= 1.12.0
* fortran_utils
* ancildat
* Test data repos contained in $GEOIPS_TESTDATA_DIR for tests to pass.

IF REQUIRED: Install base geoips package
------------------------------------------------------------
SKIP IF YOU HAVE ALREADY INSTALLED BASE GEOIPS ENVIRONMENT

If GeoIPS Base is not yet installed, follow the
[installation instructions](https://github.com/NRLMMD-GEOIPS/geoips#installation)
within the geoips source repo documentation:

Install lunarref package
------------------------
```bash

    # Assuming you followed the fully supported installation,
    # using $GEOIPS_PACKAGES_DIR and $GEOIPS_CONFIG_FILE:
    source $GEOIPS_CONFIG_FILE
    git clone https://github.com/NRLMMD-GEOIPS/fortran_utils $GEOIPS_PACKAGES_DIR/fortran_utils
    git clone https://github.com/NRLMMD-GEOIPS/lunarref $GEOIPS_PACKAGES_DIR/lunarref

    # NOTE: currently, fortran dependencies must be installed separately, initially
    # including in pyproject.toml resulted in incorrect installation paths.
    # More work required to get the pip dependencies working properly for fortran
    # installations via pyproject.toml with the poetry backend.
    pip install -e $GEOIPS_PACKAGES_DIR/fortran_utils
    pip install -e $GEOIPS_PACKAGES_DIR/lunarref
```

Test lunarref installation
--------------------------
```bash

    # Ensure GeoIPS Python environment is enabled.

    # This script will run ALL tests within this package
    $GEOIPS_PACKAGES_DIR/lunarref/tests/test_all.sh

    # Individual direct test calls, for reference
    $GEOIPS_PACKAGES_DIR/lunarref/tests/scripts/viirs.sh Lunar-Ref-IR
    $GEOIPS_PACKAGES_DIR/lunarref/tests/scripts/viirs.sh Lunar-Ref
    $GEOIPS_PACKAGES_DIR/lunarref/tests/scripts/viirs.sh Overshooting-Top

```
