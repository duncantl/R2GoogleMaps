sfcabs.html:

USE_DOCBOOK_SHORTHAND=1

include $(DYN_DOCS)/inst/Make/Makefile

TARGET_DIR=eeyore.ucdavis.edu:OmegaWeb/R2GoogleMaps/sampleDocs/

DOCS=cab1.html cab2.html cab3.html data.html cab4.html cab5.html cabShiftToggle.html \
     sfcabs.html sfcabs.pdf index.html

JAVASCRIPT=$(wildcard ../javascript/*.js)

ship: $(DOCS)
	scp $(DOCS) $(JAVASCRIPT) $(TARGET_DIR)
	scp $(JAVASCRIPT) $(TARGET_DIR)/../javascript

indexInstall: 
	scp index.html $(TARGET_DIR)
