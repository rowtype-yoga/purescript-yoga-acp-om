const decoder = new TextDecoder("utf-8");
export const decodeUtf8 = (uint8Array) => decoder.decode(uint8Array, { stream: true });
