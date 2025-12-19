// ai.js
// Configure your backend at /api/ai if you want responses.

function getLocalAIKey(){ return sessionStorage.getItem('nebula_ai_key') || ''; }

document.getElementById('save-key').onclick = () => {
  const v = document.getElementById('ai-key').value.trim();
  if (!v) { sessionStorage.removeItem('nebula_ai_key'); window.printTerminal('AI key cleared.'); return; }
  sessionStorage.setItem('nebula_ai_key', v);
  window.printTerminal('AI key saved.');
};

document.getElementById('ask-ai').onclick = async () => {
  const promptText = prompt('What do you want help with?');
  if (!promptText) return;
  window.printTerminal('🤖 AI is thinking...');
  try {
    const localKey = getLocalAIKey();
    const resp = await fetch('/api/ai', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json', 'X-Local-Key': localKey || '' },
      body: JSON.stringify({ prompt: promptText })
    });
    if (!resp.ok) throw new Error('AI request failed');
    const data = await resp.json();
    window.printTerminal('AI:\n' + (data.text || 'No response'));
  } catch (e) {
    window.printTerminal('AI error: ' + e.message);
  }
};

document.getElementById('ai-suggest-inline').onclick = async () => {
  try {
    const code = window.editor.getValue();
    const hint = '// inline suggestion: consider extracting functions for readability\n';
    window.editor.setValue(hint + code);
    window.printTerminal('Inserted inline suggestion.');
  } catch (e) {
    window.printTerminal('Inline suggest error: ' + e.message);
  }
};
