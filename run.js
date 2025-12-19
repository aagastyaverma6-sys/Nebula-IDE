// run.js — Hard-coded Judge0 language list

// Starter snippets for common languages
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
  lua: "print('Hello Nebula')",
  perl: "print \"Hello Nebula\\n\";",
  r: "cat('Hello Nebula\\n')",
  dart: "void main(){ print('Hello Nebula'); }",
  elixir: "IO.puts(\"Hello Nebula\")",
  erlang: "-module(main).\n-export([main/0]).\nmain() -> io:format(\"Hello Nebula~n\").",
  bash: "echo 'Hello Nebula'",
  vbnet: "Module Program\n  Sub Main()\n    Console.WriteLine(\"Hello Nebula\")\n  End Sub\nEnd Module",
  clojure: "(println \"Hello Nebula\")",
  crystal: "puts \"Hello Nebula\"",
  qbasic: "PRINT \"Hello Nebula\""
};

// Hard-coded Judge0 language list (IDs may vary by Judge0 instance)
window.JUDGE0_LANGS = [
  { id: 50, name: "C (GCC 9.2.0)" },
  { id: 54, name: "C++ (GCC 9.2.0)" },
  { id: 62, name: "Java (OpenJDK 13.0.2)" },
  { id: 63, name: "JavaScript (Node.js 12.14.0)" },
  { id: 71, name: "Python (3.8.1)" },
  { id: 72, name: "Ruby (2.7.0)" },
  { id: 73, name: "Rust (1.40.0)" },
  { id: 60, name: "Go (1.13.5)" },
  { id: 68, name: "PHP (7.4.1)" },
  { id: 83, name: "Swift (5.2.3)" },
  { id: 81, name: "Scala (2.13.2)" },
  { id: 78, name: "Kotlin (1.3.70)" },
  { id: 64, name: "Lua (5.3.5)" },
  { id: 67, name: "Perl (5.28.1)" },
  { id: 80, name: "R (4.0.0)" },
  { id: 85, name: "Dart (2.19.6)" },
  { id: 57, name: "Elixir (1.9.4)" },
  { id: 59, name: "Erlang (OTP 22)" },
  { id: 46, name: "Bash (5.0.0)" },
  { id: 84, name: "VB.NET (v4.0.30319)" },
  { id: 86, name: "Clojure (1.10.1)" },
  { id: 58, name: "Crystal (0.33.0)" },
  { id: 47, name: "QBasic (QB64)" } // QBasic support
];

// Populate dropdown
function populateLangDropdown() {
  const select = document.getElementById("lang");
  select.innerHTML = "";

  // JS local runner first
  const jsOpt = document.createElement("option");
  jsOpt.value = "javascript";
  jsOpt.textContent = "JavaScript (local)";
  select.appendChild(jsOpt);

  window.JUDGE0_LANGS.forEach(lang => {
    const opt = document.createElement("option");
    opt.value = String(lang.id);
    opt.textContent = lang.name;
    select.appendChild(opt);
  });

  select.value = "javascript";
}
populateLangDropdown();

// Run button
document.getElementById("run-btn").onclick = async () => {
  window.clearTerminal();
  const code = window.editor.getValue();
  const val = document.getElementById("lang").value;

  if (val === "javascript") {
    try {
      const result = (function(){ return eval(code); })();
      if (result !== undefined) window.printTerminal(String(result));
    } catch (e) {
      window.printTerminal("JS Error: " + e.message);
    }
    return;
  }

  const langId = parseInt(val, 10);
  try {
    window.printTerminal(`Submitting to Judge0 (ID ${langId})...`);
    const res = await window.runOnJudge0(code, langId);
    if (res.compile_output) window.printTerminal("Compile:\n" + res.compile_output);
    if (res.stdout) window.printTerminal("Output:\n" + res.stdout);
    if (res.stderr) window.printTerminal("Error:\n" + res.stderr);
    window.printTerminal(`Status: ${res.status.description}`);
  } catch (e) {
    window.printTerminal("Judge0 Error: " + e.message);
  }
};

// Starter snippet injection
document.getElementById("lang").addEventListener("change", (e) => {
  const val = e.target.value;
  let key = val;
  if (val === "javascript") key = "javascript";
  else if (val === "71") key = "python";
  else if (val === "47") key = "qbasic";
  else if (val === "54") key = "cpp";
  else if (val === "50") key = "c";
  else if (val === "62") key = "java";
  else if (val === "60") key = "go";
  else if (val === "73") key = "rust";
  else if (val === "72") key = "ruby";
  else if (val === "68") key = "php";

  const snippet = window.starterSnippets[key];
  if (snippet) {
    const current = window.editor.getValue();
    const looksEmpty = !current.trim();
    if (looksEmpty) window.editor.setValue(snippet);
  }
});
