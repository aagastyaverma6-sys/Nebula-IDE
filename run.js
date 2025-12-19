// run.js — Final fixed version
// - Starter snippets defined ONCE on window
// - Dynamic Judge0 language loading
// - JavaScript runs locally, others via Judge0

// ----- Starter snippets (global) -----
window.starterSnippets = {
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
  qbasic: "PRINT \"Hello Nebula\""
};

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
  const entry = window.LangRegistry?.map?.[langKey];
  const langId = entry && entry.id;
  if (!langId) {
    window.printTerminal(`No Judge0 language ID for "${langKey}".`);
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

// ----- Run button -----
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

  await runViaJudge0(code, lang);
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
  const blob = new Blob([document.getElementById('term-content').textContent], { type: 'text/plain
