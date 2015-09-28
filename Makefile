MODULES = minisat
CLEAN_MODULES = $(addprefix clean-, $(MODULES))

help:
	@echo ~~~~~~~~~~~~~~~~~~~~ SAT ~~~~~~~~~~~~~~~~~~~~
	@echo
	@echo \'make all\''                  'build all modules
	@echo \'make MODULE\''                    'build module
	@echo \'make clean-MODULE\''              'clean module
	@echo
	@echo Modules:
	@echo -n ' '$(foreach module, $(MODULES),"* $(module)\n")

all: $(MODULES)
clean: $(CLEAN_MODULES)

.PHONY: help all clean $(MODULES) $(CLEAN_MODULES)

# Information for retrieving dependencies

MINISAT_URL="https://github.com/niklasso/minisat"

# MINISAT #

minisat: $(MINISAT_DIR)
	mkdir -p $(MINISAT_DIR)/install-directory
	cd $(MINISAT_DIR) && make config prefix=$(MINISAT_DIR)/install-directory && make install

$(MINISAT_DIR):
	git clone $(MINISAT_URL)

clean-minisat:
	rm -rf $(MINISAT_DIR)/build
