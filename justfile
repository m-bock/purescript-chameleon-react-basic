build:
    spago build

test:
    spago test

gen-readme:
    node scripts/gen-readme.js
    
gen: gen-readme

format:
    purs-tidy format-in-place 'src/**/*.purs'
    purs-tidy format-in-place 'test/**/*.purs'
