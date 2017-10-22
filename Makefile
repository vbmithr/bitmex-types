all:
	jbuilder build @install

.PHONY: clean
clean:
	rm -rf _build
