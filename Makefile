all: build

build: 
	pack build talp.ipkg

run: 
	pack run talp.ipkg

watch:
	find src -iname "*.idr" | entr -sndc 'pack build talp.ipkg && echo "\n" && pack run talp.ipkg Test/Chapter1/main.ch1'

clean:
	pack clean
