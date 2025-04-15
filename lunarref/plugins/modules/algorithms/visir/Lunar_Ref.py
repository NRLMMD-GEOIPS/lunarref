# # # This source code is subject to the license referenced at
# # # https://github.com/NRLMMD-GEOIPS.

"""Lunar-Reflectance algorithm.

Data manipulation steps for standard Lunar-Reflectance imagery from VIIRS DNB

This algorithm expects reflectances, between 0 and 1,
uncorrected for Solar Zenith angle.
"""

import logging

LOG = logging.getLogger(__name__)

interface = "algorithms"
family = "list_numpy_to_numpy"
name = "Lunar_Ref"


def call(
    arrays,
    output_data_range=None,
    scale_factor=None,
    gamma_list=None,
    input_units=None,
    output_units=None,
    min_outbounds=None,
    max_outbounds=None,
    max_night_zen=None,
    norm=None,
    inverse=None,
):
    """Lunar Reflectance algorithm.

    Data manipulation steps for standard Lunar_Reflectance imagery output.

    This algorithm expects reflectances in units of albedo, between 0 and 1,
    uncorrected for solar zenith angle.

    Night product only.

    Parameters
    ----------
    data : list of numpy.ndarray
      * list of numpy.ndarray or numpy.MaskedArray of channel data,
        in order of sensor "channels" list
      * Channel data: Reflectance (DNBRef) between 0 and 100
        (however, for lighting, city lights, it goes upto 150)

    Returns
    -------
    numpy.ndarray
      numpy.ndarray or numpy.MaskedArray of appropriately scaled DNB channel data.
    """
    dnb_data = arrays[0]
    sun_zenith = arrays[1]

    from geoips.data_manipulations.info import percent_unmasked
    from geoips.data_manipulations.corrections import mask_day

    # apply the day_mask to mask daytime obs.
    dnb_data = mask_day(dnb_data, sun_zenith, max_night_zen)
    LOG.info("Percent unmasked night only %s", percent_unmasked(dnb_data))

    from geoips.data_manipulations.corrections import apply_gamma

    for gamma in gamma_list:
        dnb_data = apply_gamma(dnb_data, gamma)

    from geoips.data_manipulations.corrections import apply_scale_factor

    dnb_data = apply_scale_factor(dnb_data, scale_factor)

    from geoips.data_manipulations.corrections import apply_data_range

    dnb_data = apply_data_range(
        dnb_data,
        min_val=output_data_range[0],
        max_val=output_data_range[1],
        min_outbounds=min_outbounds,
        max_outbounds=max_outbounds,
        norm=False,
        inverse=False,
    )

    return dnb_data
