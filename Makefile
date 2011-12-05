luhn: luhn.ml
	ocamlopt -o luhn luhn.ml
clean:
	rm -rf *.cm? *.o luhn
