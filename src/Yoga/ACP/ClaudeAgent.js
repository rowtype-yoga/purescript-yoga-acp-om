import { query } from "@anthropic-ai/claude-agent-sdk";

// Allow agents to spawn Claude CLI even when the host process
// is itself running inside a Claude Code session.
delete process.env.CLAUDE_CODE;
delete process.env.CLAUDECODE;

export const queryImpl = (opts) => {
  const { prompt, ...options } = opts;
  return query({ prompt, options });
};

export const nextImpl = (iter) => iter.next();
