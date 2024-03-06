FONTNAME := Roboto-Medium

UlamSpiral.svg: UlamSpiral.hs fonts/$(FONTNAME).svg
	cabal run -- UlamSpiral -h 900 -o $@ 19

fonts/$(FONTNAME).ttf:
	@echo 'Please download the $(FONTNAME) font from fonts.google.com'
	@echo 'and place it in the fonts subdirectory'
	@false

nix/default.nix: UlamSpiral.cabal
	(cd $(@D) && cabal2nix ..) >$@

fonts/%.svg: fonts/%.ttf
	fonts/ttf2svg.sh $<

clean:
	$(RM) UlamSpiral.svg fonts/$(FONTNAME).svg
	$(RM) -r dist-newstyle
