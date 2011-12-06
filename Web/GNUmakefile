include $(OMEGA_HOME)/R/Config/GNUmakefile.Web

HTML_STYLESHEET=http://www.omegahat.org/OmegaTech.css

EXAMPLE_FILES=$(wildcard ../inst/sampleDocs/*.xml wildcard ../inst/sampleDocs/*.html ../inst/javascript/*.js)

shipExamples:
	scp $(EXAMPLE_FILES) $(TARGET_DIR)
