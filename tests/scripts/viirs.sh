#!/bin/bash

# # # This source code is subject to the license referenced at
# # # https://github.com/NRLMMD-GEOIPS.

available_products="
                    Lunar-Ref
                    Lunar-Ref-IR
                    Overshooting-Top"

for product in $available_products; do
	if [[ "$1" == "$product" ]]; then
		curr_product=$1
	fi
done
if [[ "$curr_product" == "" ]]; then
    echo ""
	echo "Usage: $0 <product>"
    echo ""
	echo "Where product one of: "
    echo "$available_products"
	exit 1
fi

run_procflow $GEOIPS_TESTDATA_DIR/test_data_viirs/data//jpss/20210525/191200/* \
          --procflow single_source \
          --reader_name viirs_netcdf \
          --product_name $curr_product \
          --compare_path "$GEOIPS_PACKAGES_DIR/lunarref/tests/outputs/viirs//<product>_image" \
         --output_formatter imagery_clean \
         --filename_formatter tc_clean_fname \
         --trackfile_parser gdeck_parser \
         --trackfiles $GEOIPS_PACKAGES_DIR/geoips_system_nrl/tests/sectors/tc_gdecks/Gio022021.dat \
         --feature_annotator tc_visir \
         --gridline_annotator tc_visir
retval=$?

exit $retval
