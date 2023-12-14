VERSION:=$(shell grep Version: DESCRIPTION|sed 's/Version: //')
NAME:=$(shell grep Package: DESCRIPTION|sed 's/Package: //')
PACKAGEFILE:=../$(NAME)_$(VERSION).tar.gz

all: $(PACKAGEFILE) README.md

.PHONY: all install localInstall

install:
	R -e 'devtools::install_github("sherrillmix/$(NAME)")'

localInstall:
	R -e 'devtools::install()'

man: R/*.R 
	R -e 'devtools::document()'
	touch man


doc: vignettes/*.Rnw data/integrations.RData data/counties.RData
	make localInstall
	R -e 'devtools::build_vignettes()'
	touch doc

README.md: README.Rmd R/*.R
	make localInstall
	R -e 'knitr::opts_chunk$$set(fig.path="tools/");knitr::knit("README.Rmd")'
	
data/integrations.RData: data-raw/makeIntegrations.R
	R -e 'source("data-raw/makeIntegrations.R",chdir=TRUE)'

data/counties.RData: data-raw/makeCounties.R
	R -e 'source("data-raw/makeCounties.R",chdir=TRUE)'

$(PACKAGEFILE): man R/*.R DESCRIPTION tests/testthat/*.R data/integrations.RData doc 
	sed -i "s/^Date:.*$$/Date: `date +%Y-%m-%d`/" DESCRIPTION
	R -e 'devtools::check();devtools::build()'
