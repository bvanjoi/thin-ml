const { generate } = require('peggy')
const path = require('node:path')
const fs = require('node:fs')

const dir = path.dirname(__filename)
const grammarPath = path.resolve(dir, './grammar.peggy')
const grammar = fs.readFileSync(grammarPath, 'utf-8')
const parserCode = generate(grammar, {
	output: 'source',
	format: 'es',
})

const parserPath = path.resolve(dir, './parser.js')
fs.writeFileSync(parserPath, parserCode)
