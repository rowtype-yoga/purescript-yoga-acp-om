import { query } from "@anthropic-ai/claude-agent-sdk";

export const queryImpl = (opts) => {
  const { prompt, ...options } = opts;
  return query({ prompt, options });
};

export const nextImpl = (iter) => iter.next();
