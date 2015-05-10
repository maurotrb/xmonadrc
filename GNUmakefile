################################################################################
ARCH              = $(shell uname -m)
OS                = $(shell uname -s | tr '[A-Z]' '[a-z]')
TARGET            = $(HOME)/.xmonad/xmonad-$(ARCH)-$(OS)
SRC               = $(shell find . -type f -name '*.hs')
DEST              = $(HOME)/bin
SANDBOX           = cabal.sandbox.config
XMONAD            = .cabal-sandbox/bin/xmonad
XMONADRC          = dist/build/xmonadrc/xmonadrc
CABAL_FLAGS       = --enable-optimization=2

################################################################################
.PHONEY: all install restart clean realclean

################################################################################
all: $(XMONADRC)

################################################################################
install: $(TARGET)
	cp -f $(XMONADRC) $(DEST)/xmonad

################################################################################
restart: install
	$(XMONAD) --restart

################################################################################
clean:
	rm -rf dist $(XMONADRC) $(CHECK) $(SANDBOX)

################################################################################
realclean:
	rm -rf .cabal-sandbox

################################################################################
$(XMONADRC): $(SRC) $(SANDBOX)
	ghc -V | grep -q 7.6.3 # Required compiler version.
	cabal build

################################################################################
$(SANDBOX):
	cabal sandbox init
	cabal install --only-dependencies $(CABAL_FLAGS)
	cabal configure $(CABAL_FLAGS)
	touch $@

################################################################################
$(TARGET): $(XMONADRC)
	mkdir -p $(dir $@)
	if [ -r $@ ]; then mv $@ $@.prev; fi
	cp -p $< $@
	cd $(dir $@) && ln -nfs $(notdir $@) xmonadrc
