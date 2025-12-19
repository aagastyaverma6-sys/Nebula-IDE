// run.js — Final dynamic Judge0 integration with QBasic support
// - Populates the language dropdown from Judge0 at runtime
// - Runs JavaScript locally; all other languages via Judge0
// - Includes starter snippets (with QBasic: PRINT "Hello Nebula")

// ----- Helpers -----
const runHistory = [];
function pushRunHistory(entry){
  runHistory.unshift(entry);
  document.getElementById('run-history').textContent =
    runHistory.slice(0,12).map((r,i)=>`${i+1}. [${r.time}] ${r.lang} — ${r.status}`).join("\n") || 'No runs yet.';
}
function nowTime(){ const d=new Date(); return d.toLocaleTimeString(); }

// Normalize Judge0 language names to clean keys for the dropdown and internal map
function normalizeLangKey(name){
  const n = (name || '').toLowerCase();
  if (n.includes('qbasic')) return 'qbasic';
  if (n.includes('c++')) return 'cpp';
  if (n.includes('objective-c')) return 'objectivec';
  if (n.startsWith('c ')) return 'c';
  if (n.startsWith('c#') || n.includes('csharp') || n.includes('c#')) return 'csharp';
  if (n.includes('f#')) return 'fsharp';
  if (n.includes('typescript')) return 'typescript';
  if (n.includes('javascript') || n.includes('node')) return 'javascript';
  if (n.includes('python')) return 'python';
  if (n.includes('java')) return 'java';
  if (n.includes('go')) return 'go';
  if (n.includes('rust')) return 'rust';
  if (n.includes('ruby')) return 'ruby';
  if (n.includes('php')) return 'php';
  if (n.includes('swift')) return 'swift';
  if (n.includes('scala')) return 'scala';
  if (n.includes('haskell')) return 'haskell';
  if (n.includes('kotlin')) return 'kotlin';
  if (n.includes('lua')) return 'lua';
  if (n.includes('perl')) return 'perl';
  if (n.includes('r ')) return 'r';
  if (n.includes('dart')) return 'dart';
  if (n.includes('elixir')) return 'elixir';
  if (n.includes('erlang')) return 'erlang';
  if (n.includes('ocaml')) return 'ocaml';
  if (n.includes('nim')) return 'nim';
  if (n.includes('groovy')) return 'groovy';
  if (n.includes('bash') || n.includes('shell')) return 'bash';
  if (n.includes('vb.net') || n.includes('vbnet')) return 'vbnet';
  if (n.includes('clojure')) return 'clojure';
  if (n.includes('crystal')) return 'crystal';
  // fallback: strip non-alphanumerics, keep simple slug
  return n.replace(/[^a-z0-9]+/g, '').slice(0, 20) || 'unknown';
}

function titleCaseKey(key){
  if (key === 'cpp') return 'C++';
  if (key === 'csharp') return 'C#';
  if (key === 'objectivec') return 'Objective-C';
  if (key === 'vbnet') return 'VB.NET';
  if (key === 'qbasic') return 'QBasic';
  return key.charAt(0).toUpperCase() + key.slice(1);
}

// Starter snippets for popular languages (Judge0 will compile/run them)
const starterSnippets = {
  javascript: "// JavaScript starter\nconsole.log('Hello Nebula');",
  python: "# Python starter\nprint('Hello Nebula')",
  cpp: "#include <iostream>\nint main(){ std::cout << \"Hello Nebula\"; return 0; }",
  c: "#include <stdio.h>\nint main(){ printf(\"Hello Nebula\\n\"); return 0; }",
  java: "class Main { public static void main(String[] args){ System.out.println(\"Hello Nebula\"); } }",
  go: "package main\nimport \"fmt\"\nfunc main(){ fmt.Println(\"Hello Nebula\") }",
  rust: "fn main(){ println!(\"Hello Nebula\"); }",
  ruby: "puts 'Hello Nebula'",
  php: "<?php echo 'Hello Nebula'; ?>",
  swift: "print(\"Hello Nebula\")",
  scala: "object Main extends App { println(\"Hello Nebula\") }",
  kotlin: "fun main(){ println(\"Hello Nebula\") }",
  haskell: "main = putStrLn \"Hello Nebula\"",
  lua: "print('Hello Nebula')",
  perl: "print \"Hello Nebula\\n\";",
  r: "cat('Hello Nebula\\n')",
  dart: "void main(){ print('Hello Nebula'); }",
  elixir: "IO.puts(\"Hello Nebula\")",
  erlang: "-module(main).\n-export([main/0]).\nmain() -> io:format(\"Hello Nebula~n\").",
  ocaml: "print_endline \"Hello Nebula\"",
  nim: "echo \"Hello Nebula\"",
  groovy: "println 'Hello Nebula'",
  bash: "echo 'Hello Nebula'",
  vbnet: "Module Program\n  Sub Main()\n    Console.WriteLine(\"Hello Nebula\")\n  End Sub\nEnd Module",
  clojure: "(println \"Hello Nebula\")",
  crystal: "puts \"Hello Nebula\"",
  // QBasic (QBASIC)
  qbasic: "PRINT \"Hello Nebula\""
};

// ----- Dynamic language discovery -----
const LangRegistry = {
  map: {},   // key -> { id, label }
  byId: {},  // id  -> { key, label }
  ready: false
};

async function loadJudge0Languages(){
  try {
    const res = await fetch('https://ce.judge0.com/languages');
    const list = await res.json();
    // Build registry
    list.forEach(lang => {
      const key = normalizeLangKey(lang.name);
      const label = lang.name; // e.g., "Python (3.11.2)"
      if (!LangRegistry.map[key]) {
        LangRegistry.map[key] = { id: lang.id, label };
      }
      LangRegistry.byId[lang.id] = { key, label };
    });

    // Ensure javascript local runner visible even if Judge0 returns Node.js differently
    LangRegistry.map['javascript'] = LangRegistry.map['javascript'] || { id: 63, label: 'JavaScript (Node.js)' };

    // Ensure QBasic appears (if API doesn’t list it for some reason)
    if (!LangRegistry.map['qbasic']) {
      // Common Judge0 category label; id will be filled if present in API above
      LangRegistry.map['qbasic'] = { id: LangRegistry.map['qbasic']?.id || null, label: 'QBasic' };
    }

    // Populate dropdown
    const langSelect = document.getElementById('lang');
    langSelect.innerHTML = '';
    // Place JavaScript first (local)
    const jsOpt = document.createElement('option');
    jsOpt.value = 'javascript';
    jsOpt.textContent = 'JavaScript (local)';
    langSelect.appendChild(jsOpt);

    // Add rest (alphabetical)
    const keys = Object.keys(LangRegistry.map)
      .filter(k => k !== 'javascript')
      .sort((a,b) => titleCaseKey(a).localeCompare(titleCaseKey(b)));

    keys.forEach(k => {
      const opt = document.createElement('option');
      opt.value = k;
      const label = LangRegistry.map[k].label || titleCaseKey(k);
      opt.textContent = label;
      langSelect.appendChild(opt);
    });

    // Default selection
    langSelect.value = 'javascript';
    LangRegistry.ready = true;

  } catch (e) {
    // Fallback minimal set if API fails
    const langSelect = document.getElementById('lang');
    if (langSelect.querySelectorAll('option').length === 0) {
      langSelect.innerHTML = `
        <option value="javascript">JavaScript (local)</option>
        <option value="python">Python</option>
        <option value="cpp">C++</option>
        <option value="java">Java</option>
        <option value="go">Go</option>
        <option value="rust">Rust</option>
        <option value="ruby">Ruby</option>
        <option value="php">PHP</option>
        <option value="qbasic">QBasic</option>
      `;
    }
    window.printTerminal('Language fetch failed; using fallback list.');
  }
}

// Call on load
loadJudge0Languages();

// ----- Runner -----
async function runViaJudge0(code, langKey) {
  // Block if we don’t have an ID for the selected lang
  const entry = LangRegistry.map[langKey] || null;
  const langId = entry && entry.id;

  if (!langId) {
    window.printTerminal(`No Judge0 language ID for "${langKey}". Try a different runtime or refresh.`);
    pushRunHistory({ lang: langKey, status: 'No ID', time: nowTime() });
    return { used: true, ok: false };
  }

  try {
    window.printTerminal(`Submitting to Judge0 (${entry.label || langKey})...`);
    const res = await window.runOnJudge0(code, langId);
    if (res.compile_output) window.printTerminal("Compile:\n" + res.compile_output);
    if (res.stdout) window.printTerminal("Output:\n" + res.stdout);
    if (res.stderr) window.printTerminal("Error:\n" + res.stderr);
    window.printTerminal(`Status: ${res.status.description} | time: ${res.time}s | memory: ${res.memory}KB`);
    pushRunHistory({ lang: langKey, status: res.status.description, time: nowTime() });
    return { used: true, ok: true };
  } catch (e) {
    window.printTerminal("Judge0 Error: " + e.message);
    pushRunHistory({ lang: langKey, status: "Error", time: nowTime() });
    return { used: true, ok: false };
  }
}

// Main run button
document.getElementById("run-btn").onclick = async () => {
  window.clearTerminal();
  const code = window.editor.getValue();
  const lang = document.getElementById("lang").value;

  if (lang === "javascript") {
    // Local JS runner
    try {
      const result = (function(){ return eval(code); })();
      if (result !== undefined) window.printTerminal(String(result));
      pushRunHistory({ lang, status: 'OK', time: nowTime() });
    } catch (e) {
      window.printTerminal('JS Error: ' + (e.message || String(e)));
      pushRunHistory({ lang, status: 'Error', time: nowTime() });
    }
    return;
  }

  // Judge0 for all other languages
  const j = await runViaJudge0(code, lang);
  if (!j.used) {
    window.printTerminal('No runner used.');
    pushRunHistory({ lang, status: 'No runner', time: nowTime() });
  }
};

// Clear/copy/download terminal actions
document.getElementById('clear-btn').addEventListener('click', () => window.clearTerminal());
document.getElementById('copy-terminal').addEventListener('click', async () => {
  try {
    await navigator.clipboard.writeText(document.getElementById('term-content').textContent);
    window.printTerminal('Copied terminal.');
  } catch {
    window.printTerminal('Copy failed.');
  }
});
document.getElementById('download-log').addEventListener('click', () => {
  const blob = new Blob([document.getElementById('term-content').textContent], { type: 'text/plain' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url; a.download = 'nebula-terminal.log'; a.click();
  URL.revokeObjectURL(url);
});

// ----- Optional: set starter snippet on language change -----
document.getElementById('lang').addEventListener('change', (e) => {
  const lang = e.target.value;
  const snippet = starterSnippets[lang];
  if (snippet) {
    const current = window.editor.getValue();
    const looksEmpty = !current.trim() || current.startsWith('//') || current.startsWith('#');
    if (looksEmpty) window.editor.setValue(snippet);
  }
});
