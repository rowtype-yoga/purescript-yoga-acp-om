import { createServer } from "node:http";
import { execFile, spawn } from "node:child_process";

let server = null;

export const startServerImpl = () =>
  new Promise((resolve, reject) => {
    server = createServer(handleRequest);
    server.listen(0, () => resolve(server.address().port));
    server.on("error", reject);
  });

export const stopServerImpl = () =>
  new Promise((resolve) => {
    if (server) {
      server.close(() => resolve());
      server = null;
    } else {
      resolve();
    }
  });

export const isClaudeAvailableImpl = () =>
  new Promise((resolve) => {
    execFile("which", ["claude"], (err) => resolve(!err));
  });

export const filteredEnvImpl = (prefix) => Object.fromEntries(Object.entries(process.env).filter(([k]) => !k.startsWith(prefix)));

function handleRequest(req, res) {
  const url = new URL(req.url, `http://${req.headers.host}`);
  if (req.method === "GET" && url.pathname === "/ping") {
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end("{}");
  } else if (req.method === "GET" && url.pathname === "/agents") {
    handleListAgents(res, url.searchParams);
  } else if (req.method === "POST" && url.pathname === "/runs") {
    handleCreateRun(req, res);
  } else {
    res.writeHead(404);
    res.end();
  }
}

function handleListAgents(res, params) {
  const agents = [
    {
      name: "claude-code",
      description: "Claude Code",
    },
  ];
  const offset = parseInt(params.get("offset") || "0", 10);
  const limit = parseInt(params.get("limit") || "10", 10);
  const paged = agents.slice(offset, offset + limit);
  res.writeHead(200, { "Content-Type": "application/json" });
  res.end(JSON.stringify(paged));
}

function handleCreateRun(req, res) {
  let body = "";
  req.on("data", (chunk) => {
    body += chunk;
  });
  req.on("end", () => {
    const request = JSON.parse(body);
    const message = request.input?.[0]?.parts?.[0]?.content || "hello";
    const runId = "run-" + Date.now();

    res.writeHead(200, {
      "Content-Type": "text/event-stream",
      "Cache-Control": "no-cache",
      Connection: "keep-alive",
    });

    sendSSE(res, "run.created", makeRun(runId, "created"));

    const env = Object.fromEntries(
      Object.entries(process.env).filter(
        ([k]) => !k.startsWith("CLAUDE"),
      ),
    );

    const proc = spawn("claude", ["-p", message, "--max-turns", "1"], {
      env,
      stdio: ["ignore", "pipe", "pipe"],
    });

    let stdout = "";
    proc.stdout.on("data", (chunk) => {
      stdout += chunk.toString();
    });

    proc.on("close", (code) => {
      const text = code !== 0 ? "Error: claude exited with code " + code : stdout.trim();
      const msg = makeMessage("assistant", text);
      sendSSE(res, "message.completed", msg);
      sendSSE(res, "run.completed", makeRun(runId, "completed", [msg]));
      res.end();
    });
  });
}

function makeRun(runId, status, output) {
  return {
    agent_name: "claude-code",
    run_id: runId,
    status,
    output: output || null,
    error: null,
    session_id: null,
    await_request: null,
    created_at: null,
    finished_at: null,
  };
}

function makeMessage(role, content) {
  return {
    role,
    parts: [
      {
        content_type: "text/plain",
        content,
        content_url: null,
        content_encoding: null,
        name: null,
        metadata: null,
      },
    ],
  };
}

function sendSSE(res, event, data) {
  res.write("event: " + event + "\ndata: " + JSON.stringify(data) + "\n\n");
}
