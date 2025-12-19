// judge0.js
// Uses public CE instance. For production, proxy via your backend.

const J0_BASE = "https://ce.judge0.com";
const J0_SUBMIT = J0_BASE + "/submissions/?base64_encoded=true&wait=false";
const J0_STATUS = (token) => J0_BASE + "/submissions/" + token + "?base64_encoded=true";

async function runOnJudge0(code, languageId, stdin = "") {
  const payload = {
    source_code: btoa(unescape(encodeURIComponent(code))),
    language_id: languageId,
    stdin: btoa(unescape(encodeURIComponent(stdin)))
  };

  const submitResp = await fetch(J0_SUBMIT, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(payload)
  });
  if (!submitResp.ok) throw new Error("Judge0 submit failed: " + submitResp.status);
  const { token } = await submitResp.json();

  // Poll until finished
  while (true) {
    const r = await fetch(J0_STATUS(token));
    const data = await r.json();
    if (data.status && data.status.id >= 3) {
      const decode = (x) => (x ? decodeURIComponent(escape(atob(x))) : "");
      return {
        status: data.status,
        stdout: decode(data.stdout),
        stderr: decode(data.stderr),
        compile_output: decode(data.compile_output),
        time: data.time,
        memory: data.memory
      };
    }
    await new Promise(res => setTimeout(res, 1000));
  }
}

window.runOnJudge0 = runOnJudge0;
