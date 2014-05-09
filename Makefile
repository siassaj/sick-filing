TARGET		= filing
SYSTEM		= sick-filing

all: clean manifest
	buildapp --output "$(TARGET)" \
		--manifest-file quicklisp-manifest.txt \
		--load-system $(SYSTEM) \
		--entry "$(SYSTEM):main"

manifest:
	sbcl --no-userinit --no-sysinit --non-interactive \
		--load ~/quicklisp/setup.lisp \
		--eval '(ql:quickload "$(SYSTEM)")' \
		--eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")'
clean:
	-rm quicklisp-manifest.txt $(TARGET)
