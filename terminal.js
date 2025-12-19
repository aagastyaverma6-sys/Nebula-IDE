// terminal.js

window.clearTerminal = () => {
  document.getElementById("term-content").textContent = "> Cleared...\n";
};
window.printTerminal = (text) => {
  const term = document.getElementById("term-content");
  term.textContent += text + "\n";
  term.scrollTop = term.scrollHeight;
};
