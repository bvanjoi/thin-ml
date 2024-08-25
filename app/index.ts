import {
	EditorView,
	crosshairCursor,
	drawSelection,
	dropCursor,
	highlightActiveLine,
	highlightActiveLineGutter,
	highlightSpecialChars,
	keymap,
	lineNumbers,
	rectangularSelection,
} from '@codemirror/view'
export { EditorView } from '@codemirror/view'
import { EditorState } from '@codemirror/state'
export { EditorState } from '@codemirror/state'
import { defaultKeymap, history, historyKeymap } from '@codemirror/commands'
import {
	bracketMatching,
	defaultHighlightStyle,
	foldGutter,
	foldKeymap,
	indentOnInput,
	syntaxHighlighting,
} from '@codemirror/language'
import { exec, printTypeWithName } from '../src/index'

const basicSetup = [
	lineNumbers(),
	highlightActiveLineGutter(),
	highlightSpecialChars(),
	history(),
	foldGutter(),
	drawSelection(),
	dropCursor(),
	EditorState.allowMultipleSelections.of(true),
	indentOnInput(),
	syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
	bracketMatching(),
	rectangularSelection(),
	crosshairCursor(),
	highlightActiveLine(),
	keymap.of([...defaultKeymap, ...historyKeymap, ...foldKeymap]),
]
import './index.css'

const container = document.getElementById('cm-editor')
if (!container) {
	throw new Error('No container element found')
}

const initDoc = `let a = 1;
a + 1;
a - 2;
a * 3;
a == 4;
a > 5;
(a + 1) * 2 + 3;
(a + 1) * (2 + 3);

let f x = x;
f 1;
f 1.; 
f true;
f false;

let f x = x + 1;
f 1;

let rec fac n = if n <= 0 then 1 else (n * (fac (n-1)));
fac 1;
`

const output = document.getElementById('cm-output')
if (!output) {
	throw new Error('No output element found')
}
output.style.width = '800px'
output.style.height = '800px'

function getTypes(code: string): [string, string] {
	try {
		const [tys, errors] = exec(code)
		if (errors.length) {
			const errorMsg = errors.map(e => e.message)
			return ['', errorMsg.join('\n')]
		}
		return [tys.map(([name, ty]) => printTypeWithName(name, ty)).join('\n'), '']
	} catch (e) {
		return ['', (e as unknown as Error).message]
	}
}
const render = (input: string) => {
	const ERROR_CLASS = 'error'
	const [tyInfo, errors] = getTypes(input)
	if (errors.length > 0) {
		output.textContent = errors
		output.classList.add(ERROR_CLASS)
	} else {
		output.textContent = tyInfo
		output.classList.remove(ERROR_CLASS)
	}
}

const ext = EditorView.updateListener.of(update => {
	if (!update.docChanged) {
		return
	}
	render(update.state.doc.toString())
})

const state = EditorState.create({
	doc: initDoc,
	extensions: [...basicSetup, ext],
})
const view = new EditorView({
	state,
	parent: container,
})

render(view.state.doc.toString())
