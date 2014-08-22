all: src/Geom.elm
	elm --make --src-dir src/ --only-js Geom.elm

clean:
	rm -r build/ cache/
