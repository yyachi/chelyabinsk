PACKAGENAME:=$(shell basename `pwd`)
VERSION:=1.0
TARGET:=PACKAGENAME_$(VERSION).tar.gz
RFILE:=$(wildcard R/*.R)
RDOCFILE:=$(wildcard man/*.Rd)

$(TARGET): DESCRIPTION NAMESPACE $(RDOCFILE)
	R CMD build ../$(PACKAGENAME)

NAMESPACE: $(RFILE)
	Rscript -e "devtools::document()"

.PHONY: rdoc
rdoc:
	Rscript -e "devtools::document()"

.PHONY: test
test:
	R CMD check ../$(PACKAGENAME)

.PHONY: build
rdoc:
	R CMD build ../$(PACKAGENAME)

.PHONY: install
install:
	R CMD install $(TARGET)
