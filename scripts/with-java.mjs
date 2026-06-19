import { spawn } from "node:child_process";
import { withJavaEnv } from "./java-home.mjs";

const isWindows = process.platform === "win32";
const [command, ...args] = process.argv.slice(2);

if (!command) {
  console.error("usage: node scripts/with-java.mjs <command> [...args]");
  process.exit(1);
}

let env;
try {
  env = withJavaEnv();
} catch (error) {
  console.error(`[java] ${error.message}`);
  process.exit(1);
}

const child = spawn(command, args, {
  env,
  shell: isWindows,
  stdio: "inherit",
});

child.on("exit", (code, signal) => {
  if (signal) {
    process.kill(process.pid, signal);
    return;
  }

  process.exit(code ?? 0);
});

child.on("error", error => {
  console.error(`[java] Failed to start ${command}: ${error.message}`);
  process.exit(1);
});
