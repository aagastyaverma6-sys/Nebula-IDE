// run.js — Final complete version with dynamic Judge0 language loading

// Starter snippets for some common langs (others will just start empty)
window.starterSnippets = {
  javascript: "// JavaScript starter\nconsole.log('Hello Nebula');",
  python: "# Python starter\nprint('Hello Nebula')",
  qbasic: "PRINT \"Hello Nebula\""
};

// Registry of Judge0 languages
window.LangRegistry = { map: {}, byId: {} };

// ----- Load all Judge0 languages and populate dropdown -----
async function loadJudge0Languages() {
  try {
    const res = await fetch("https://ce.judge0.com/languages");
    const langs = await res.json();

    const select = document.getElementById("lang");
    select.innerHTML = "";

    // Always put JS first (local runner)
    const jsOpt = document.createElement("option");
    jsOpt.value = "javascript";
    jsOpt.textContent = "JavaScript (local)";
    select.appendChild(jsOpt);

    langs.forEach(lang => {
      const key = lang.name.toLowerCase();
      const opt = document.createElement("option");
      opt.value = key;
      opt.textContent = lang.name;
      select.appendChild(opt);

      window.LangRegistry.map[key] = { id: lang.id, label: lang.name };
      window.LangRegistry.byId[lang.id] = { key, label: lang.name };
    });

    select.value = "javascript";
  } catch (err) {
    window.printTerminal("Failed to load Judge0 languages: " + err.message);
    // Fallback minimal list
    const select = document.getElementById("lang");
    select.innerHTML = `
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
}
loadJudge0Languages();

// ----- Run history helpers -----
const runHistory = [];
function pushRunHistory(entry){
  runHistory.unshift(entry);
  document.getElementById('run-history').textContent =
    runHistory.slice(0,12).map((r,i)=>`${i+1}. [${r.time}] ${r.lang} — ${r.status}`).join("\n") || 'No runs yet.';
}
function nowTime(){ const d=new Date(); return d.toLocaleTimeString(); }

// ----- Judge0 runner -----
async function runViaJudge0(code, langKey) {
  const entry = window.LangRegistry.map[langKey];
  const langId = entry && entry.id;
  if (!langId) {
    window.printTerminal(`No Judge0 language ID for "${langKey}".`);
    pushRunHistory({ lang: langKey, status: 'No ID', time: nowTime() });
    return;
  }
  try {
    window.printTerminal(`Submitting to Judge0 (${entry.label})...`);
    const res = await window.runOnJudge0(code, langId);
    if (res.compile_output) window.printTerminal("Compile:\n" + res.compile_output);
    if (res.stdout) window.printTerminal("Output:\n" + res.stdout);
    if (res.stderr) window.printTerminal("Error:\n" + res.stderr);
    window.printTerminal(`Status: ${res.status.description} | time: ${res.time}s | memory: ${res.memory}KB`);
    pushRunHistory({ lang: langKey, status: res.status.description, time: nowTime() });
  } catch (e) {
    window.printTerminal("Judge0 Error: " + e.message);
    pushRunHistory({ lang: langKey, status: "Error", time: nowTime() });
  }
}

// ----- Run button -----
document.getElementById("run-btn").onclick = async () => {
  window.clearTerminal();
  const code = window.editor.getValue();
  const langKey = document.getElementById("lang").value;

  if (langKey === "javascript") {
    try {
      const result = (function(){ return eval(code); })();
      if (result !== undefined) window.printTerminal(String(result));
      pushRunHistory({ lang: langKey, status: 'OK', time: nowTime() });
    } catch (e) {
      window.printTerminal("JS Error: " + e.message);
      pushRunHistory({ lang: langKey, status: 'Error', time: nowTime() });
    }
    return;
  }

  await runViaJudge0(code, langKey);
};

// ----- Terminal actions -----
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

// ----- Starter snippet injection on language change -----
document.getElementById('lang').addEventListener('change', (e) => {
  const lang = e.target.value;
  const snippet = window.starterSnippets[lang];
  if (snippet) {
    const current = window.editor.getValue();
    const looksEmpty = !current.trim() || current.startsWith('//') || current.startsWith('#') || current.startsWith('PRINT');
    if (looksEmpty) window.editor.setValue(snippet);
  }
});
