// run.js

const JUDGE0_LANG_MAP = {
  javascript: 63, // Node.js (unused for Judge0 here; JS runs locally)
  python: 71,
  c: 50,
  cpp: 54,
  java: 62,
  go: 60,
  rust: 73,
  ruby: 72,
  php: 68
};

const runHistory = [];
function pushRunHistory(entry){
  runHistory.unshift(entry);
  document.getElementById('run-history').textContent =
    runHistory.slice(0,12).map((r,i)=>`${i+1}. [${r.time}] ${r.lang} — ${r.status}`).join("\n") || 'No runs yet.';
}
function nowTime(){ const d=new Date(); return d.toLocaleTimeString(); }

async function runViaJudge0(code, langKey) {
  const langId = JUDGE0_LANG_MAP[langKey];
  if (!langId) return { used:false };
  try {
    window.printTerminal("Submitting to Judge0...");
    const res = await window.runOnJudge0(code, langId);
    if (res.compile_output) window.printTerminal("Compile:\n"+res.compile_output);
    if (res.stdout) window.printTerminal("Output:\n"+res.stdout);
    if (res.stderr) window.printTerminal("Error:\n"+res.stderr);
    window.printTerminal(`Status: ${res.status.description} | time: ${res.time}s | memory: ${res.memory}KB`);
    pushRunHistory({ lang: langKey, status: res.status.description, time: nowTime() });
    return { used:true, ok:true };
  } catch(e){
    window.printTerminal("Judge0 Error: "+e.message);
    pushRunHistory({ lang: langKey, status:"Error", time: nowTime() });
    return { used:true, ok:false };
  }
}

document.getElementById("run-btn").onclick = async () => {
  window.clearTerminal();
  const code = window.editor.getValue();
  const lang = document.getElementById("lang").value;

  if (lang === "javascript") {
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

  const j = await runViaJudge0(code, lang);
  if (!j.used) {
    window.printTerminal('No runner for ' + lang + '. Add mapping.');
    pushRunHistory({ lang, status: 'No runner', time: nowTime() });
  }
};

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
