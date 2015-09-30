VERSION:=$(shell grep Version: DESCRIPTION|sed 's/Version: //')
NAME:=$(shell grep Package: DESCRIPTION|sed 's/Package: //')
PACKAGEFILE:=../$(NAME)_$(VERSION).tar.gz

all: $(PACKAGEFILE) README.md

.PHONY: all install

install:
	R -e 'devtools::install_github("eclarke/violin_point")'

man: R/*.R
	R -e 'devtools::document()'
	touch man

README.md: README.Rmd
	R -e 'knitr::opts_chunk$$set(fig.path="README_files/");knitr::knit("README.Rmd")'

#inst/doc: vignettes/*.Rnw
	#R -e 'devtools::build_vignettes()'
	#touch inst/doc

	
#inst/doc
$(PACKAGEFILE): man R/*.R DESCRIPTION tests/testthat/tests.R
	R -e 'devtools::check();devtools::build()'
