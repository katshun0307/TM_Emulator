builder = dune

%.exe : %.ml
	$(builder) build $@

%.cmxa : %.ml
	$(builder) build $@
