 | # # # Distribution Statement A. Approved for public release. Distribution unlimited.
 | # # #
 | # # # Author:
 | # # # Naval Research Laboratory, Marine Meteorology Division
 | # # #
 | # # # This program is free software: you can redistribute it and/or modify it under
 | # # # the terms of the NRLMMD License included with this program. This program is
 | # # # distributed WITHOUT ANY WARRANTY; without even the implied warranty of
 | # # # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the included license
 | # # # for more details. If you did not receive the license, for more information see:
 | # # # https://github.com/U-S-NRL-Marine-Meteorology-Division/

Lunar Reflectance
+++++++++++++++++
The :func:`lunref <geoalgs.lunref>` function is used to calculate approcimate reflectances for
VIIRS Day/Night Band radiances.  This function takes into account lunar phase, lunar zenith
angle, lunar distance, and several other parameters in its calculation.  This is performed
using lookup tables which are available in `geoalgs/src/lunarref/dat/`.

.. note:: Need to add information here about the lookup tables themselves.
          Should probably also add a reference to the lunar reflectance paper.

.. function:: geoalgs.lunarref(in_dnb_rad, solar_zenith, lunar_zenith, yyyymmddhh, minute, lunar_phase[, lines, samples, in_dat_path])

    :noindex:
    :param in_dnb_rad: 2-D array of DNB radiances *(lines, samples)*
    :type in_dnb_rad: array of floats
    :param solar_zenith: 2-D array of solar zenith angles *(lines, samples)*
    :type solar_zenith: array of floats
    :param lunar_zenith: 2-D array of lunar zenith angles *(lines, samples)*
    :type lunar_zenith: array of floats
    :param yyyymmddhh: Year, month, day, hour as a long
    :type yyyymmddhh: long
    :param minute: Number of minutes past the hour
    :type minute: int
    :param lunar_phase: Phase angle of the moon (available in DNB data)
    :type lunar_phase: float
    :param lines: Number of lines in the input `in_dnb_rad` dataset
    :type lines: int or assumed
    :param samples: Number of samples in the input `in_dnb_rad` dataset
    :type samples: int or assumed
