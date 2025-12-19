// app.js — Final Refined Version

let __files = [];
let __activeId = null;
let __nextId = 1;

/* Starter snippets per language */
const starterSnippets = {
  javascript: "// JavaScript starter\nconsole.log('Hello Nebula');",
  python: "# Python starter\nprint('Hello Nebula')",
  cpp: "#include <iostream>\nint main(){ std::cout << \"Hello Nebula\"; }",
  java: "class Main { public static void main(String[] args){ System.out.println(\"Hello Nebula\"); } }",
  go: "package main\nimport \"fmt\"\nfunc main(){ fmt.Println(\"Hello Nebula\") }",
  rust: "fn main(){ println!(\"Hello Nebula\"); }",
  ruby: "puts 'Hello Nebula'",
  php: "<?php echo 'Hello Nebula'; ?>"
};

/* Tabs rendering */
function renderTabs(){
  const wrap = document.getElementById('file-tabs');
  wrap.innerHTML = '';
  __files.forEach(f => {
    const el = document.createElement('div');
    el.className = 'file-tab' + (f.id === __activeId ? ' active' : '');
    el.innerHTML = `<span class="name">${f.name}</span> <span class="close">✖</span>`;
    el.querySelector('.close').onclick = (e) => {
      e.stopPropagation();
      __files = __files.filter(x => x.id !== f.id);
      if (__files.length) { __activeId = __files[0].id; loadActiveToEditor(); }
      else { __activeId = null; window.editor.setValue(''); }
      renderTabs();
    };
    el.onclick = () => { __activeId = f.id; loadActiveToEditor(); renderTabs(); };
    wrap.appendChild(el);
  });
}

/* Create/load files */
function createFile({ name, language }){
  const f = { id: __nextId++, name, language, content: starterSnippets[language] || '' };
  __files.push(f);
  __activeId = f.id;
  renderTabs();
  loadActiveToEditor();
}
function loadActiveToEditor(){
  const f = __files.find(x => x.id === __activeId);
  if (!f) return;
  window.editor.setValue(f.content || '');
  const modelLang = (f.language === 'python') ? 'python' : 'javascript';
  monaco.editor.setModelLanguage(window.__monacoEditor.getModel(), modelLang);
  document.getElementById('lang').value = f.language;
}
function updateActiveFromEditor(){
  const f = __files.find(x => x.id === __activeId);
  if (!f) return;
  f.content = window.editor.getValue();
  f.language = document.getElementById('lang').value;
}

/* Panel switching */
document.querySelectorAll('.tab').forEach(t => {
  t.addEventListener('click', () => {
    document.querySelectorAll('.tab').forEach(x => x.classList.remove('active'));
    t.classList.add('active');
    document.querySelectorAll('.tool-panel').forEach(p => p.classList.remove('show'));
    const panel = document.getElementById(t.dataset.panel);
    if (panel) panel.classList.add('show');
  });
});

/* Splitter drag to resize terminal */
(function(){
  const splitter = document.getElementById('splitter');
  const terminal = document.getElementById('terminal');
  let dragging = false;
  splitter.addEventListener('mousedown', () => { dragging = true; document.body.style.cursor = 'row-resize'; });
  window.addEventListener('mouseup', () => { dragging = false; document.body.style.cursor = ''; });
  window.addEventListener('mousemove', (e) => {
    if (!dragging) return;
    const rect = document.querySelector('.editor-wrap').getBoundingClientRect();
    const newHeight = Math.max(120, rect.bottom - e.clientY);
    terminal.style.height = newHeight + 'px';
  });
})();

/* Monaco init */
require.config({ paths: { vs: 'https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.44.0/min/vs' }});
require(['vs/editor/editor.main'], function() {
  monaco.editor.defineTheme('nebula-dark', {
    base: 'vs-dark', inherit: true,
    rules: [
      { token: 'keyword', foreground: '38bdf8', fontStyle: 'bold' },
      { token: 'string', foreground: '22c55e' },
      { token: 'number', foreground: 'facc15' },
      { token: 'comment', foreground: '64748b', fontStyle: 'italic' }
    ],
    colors: {
      'editor.background': '#091125',
      'editorCursor.foreground': '#38bdf8',
      'editor.selectionBackground': '#1e293b',
      'editor.lineHighlightBackground': '#0f172a55'
    }
  });

  const monacoEditor = monaco.editor.create(document.getElementById('editor'), {
    value: starterSnippets.javascript,
    language: 'javascript',
    theme: 'nebula-dark',
    fontSize: 13,
    minimap: { enabled: true },
    automaticLayout: true,
    lineNumbers: 'on',
    wordWrap: 'off'
  });

  window.__monacoEditor = monacoEditor;
  window.editor = {
    getValue: () => monacoEditor.getValue(),
    setValue: v => monacoEditor.setValue(v),
    getModel: () => monacoEditor.getModel()
  };

  createFile({ name: 'main.js', language: 'javascript' });
  monacoEditor.onDidChangeModelContent(() => updateActiveFromEditor());
});

/* View toggles */
document.getElementById('toggle-minimap').addEventListener('change', (e) => {
  window.__monacoEditor.updateOptions({ minimap: { enabled: e.target.checked } });
});
document.getElementById('toggle-line-numbers').addEventListener('change', (e) => {
  window.__monacoEditor.updateOptions({ lineNumbers: e.target.checked ? 'on' : 'off' });
});
document.getElementById('toggle-word-wrap').addEventListener('change', (e) => {
  window.__monacoEditor.updateOptions({ wordWrap: e.target.checked ? 'on' : 'off' });
});

/* Lang select */
document.getElementById('lang').addEventListener('change', (e) => {
  const lang = e.target.value;
  const modelLang = (lang === 'python') ? 'python' : 'javascript';
  monaco.editor.setModelLanguage(window.__monacoEditor.getModel(), modelLang);
  const f = __files.find(x => x.id === __activeId);
  if (f) {
    f.language = lang;
    if (!f.content || f.content.trim() === '' || f.content.startsWith('//') || f.content.startsWith('#')) {
      f.content = starterSnippets[lang] || '';
      window.editor.setValue(f.content);
    }
  }
});

/* File actions */
document.getElementById('new-file').onclick = () => createFile({ name:`untitled-${__nextId}.js`, language:'javascript' });
document.getElementById('open-sample').onclick = () => createFile({ name:'sample.py', language:'python' });
document.getElementById('upload-file').onclick = () => document.getElementById('upload-input').click();
document.getElementById('upload-input').onchange = (e) => {
  const file = e.target.files[0];
  if (!file) return;
  const reader = new FileReader();
  reader.onload = () => {
    const ext = (file.name.split('.').pop() || '').toLowerCase();
    const lang = ext === 'py' ? 'python' : 'javascript';
    createFile({ name:file.name, language:lang });
    window.editor.setValue(String(reader.result));
  };
  reader.readAsText(file);
};
document.getElementById('save-file').onclick = () => {
  const f = __files.find(x => x.id === __activeId);
  const name = (f && f.name) || 'nebula-file.txt';
  const blob = new Blob([window.editor.getValue()], { type:'text/plain' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a'); a.href=url; a.download=name; a.click();
  URL.revokeObjectURL(url);
};
