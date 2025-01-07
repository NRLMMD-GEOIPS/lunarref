# # # This source code is protected under the license referenced at
# # # https://github.com/NRLMMD-GEOIPS.

"""Overshooting-Top algorithm.

This algorithm expects two VIIRS channels (DNBRef and M16BT) for a RGB image
"""

import logging

LOG = logging.getLogger(__name__)

interface = "algorithms"
family = "list_numpy_to_numpy"
name = "Overshooting_Top"


def call(arrays):
    """Overshooting Top algorithm.

    Data manipulation steps for "rgb" product algorithm.

    This algorithm expects DNBRef in reflectance and
    M16BT Brightness Temperatures in units of degrees Kelvin,
    and returns red green and blue gun arrays.

    Parameters
    ----------
    data : list of numpy.ndarray
        * list of numpy.ndarray or numpy.MaskedArray of channel data,
          in order of sensor "channels" list
        * Degrees Kelvin

    Returns
    -------
    numpy.ndarray
        numpy.ndarray or numpy.MaskedArray of qualitative RGBA image output
    """
    ch1 = arrays[0]  # Red gun: DNBRef (50-100)
    ch2 = arrays[0]  # Green gun: DNBRef (50-100)
    ch3 = arrays[1]  # Blue gun: M16BT (180-230K)

    data_range1 = [50, 115]  # DNBRef reflectance range
    data_range2 = [180, 230]  # M16BT range

    from geoips.data_manipulations.corrections import apply_data_range, apply_gamma

    red = ch1
    red = apply_data_range(
        red,
        min_val=data_range1[0],
        max_val=data_range1[1],
        min_outbounds="crop",
        max_outbounds="crop",
        norm=True,
        inverse=False,
    )  # need inverse option?

    red = apply_gamma(red, 1.0)

    grn = ch2
    grn = apply_data_range(
        grn,
        min_val=data_range1[0],
        max_val=data_range1[1],
        min_outbounds="crop",
        max_outbounds="crop",
        norm=True,
        inverse=False,
    )

    grn = apply_gamma(grn, 1.0)

    blu = ch3
    blu = apply_data_range(
        blu,
        min_val=data_range2[0],
        max_val=data_range2[1],
        min_outbounds="crop",
        max_outbounds="crop",
        norm=True,
        inverse=True,
    )  # a good TC case to test the option
    # norm=True, inverse=False)

    blu = apply_gamma(blu, 1.0)

    from geoips.image_utils.mpl_utils import alpha_from_masked_arrays, rgba_from_arrays

    alp = alpha_from_masked_arrays([red, grn, blu])
    rgba = rgba_from_arrays(red, grn, blu, alp)

    return rgba
