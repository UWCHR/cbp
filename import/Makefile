# set ts=4 softtabstop=0 expandtab sw=4 si fe=utf-8
#
# Authors:     PN
# Maintainers: PN
# Copyright:   2022, UWCHR, GPL v2 or later
# =========================================
# cbp/import/Makefile

SHELL := bash 

.PHONY: all

# This could probably be written better to specify outputs
all: \
		input/us_border_patrol_monthly_encounters_fy2000-fy2020-508.pdf
	page=1 ; while [[ $$page -le 21 ]] ; do \
		((fy=$$page + 1999)) ; \
		pdftotext -f $$page -l $$page -layout $< $(addprefix frozen/usbp_monthly_encounters_fy,$$fy.csv); \
		sed -i 's/^ *//;s/ \{2,\}/|/g;' $(addprefix frozen/usbp_monthly_encounters_fy,$$fy.csv); \
		((page = page + 1)) ; \
	done
	Rscript --vanilla src/convert.R

clean: 
		-rm -r output/*


# done.