builder = dune

%.exe : %.ml
	$(builder) build $@
