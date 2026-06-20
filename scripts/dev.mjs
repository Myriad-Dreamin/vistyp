import { spawn } from "node:child_process";

const isWindows = process.platform === "win32";
const viteArgs = process.argv.slice(2);
const children = new Set();
let shuttingDown = false;
let exitTimer = undefined;

function start(name, command, args) {
  const child = spawn(command, args, {
    stdio: "inherit",
    shell: isWindows,
  });

  children.add(child);

  child.on("error", error => {
    console.error(`[dev] Failed to start ${name}: ${error.message}`);
    shutdown(1);
  });

  child.on("exit", (code, signal) => {
    children.delete(child);

    if (shuttingDown) {
      return;
    }

    const reason = signal ? `signal ${signal}` : `code ${code ?? 0}`;
    console.error(`[dev] ${name} exited with ${reason}; stopping dev server.`);
    shutdown(code ?? 1);
  });

  return child;
}

function stop(child) {
  if (child.exitCode !== null || child.signalCode !== null) {
    return;
  }

  if (isWindows) {
    spawn("taskkill", ["/pid", String(child.pid), "/T", "/F"], {
      stdio: "ignore",
      shell: true,
    });
  } else {
    child.kill("SIGTERM");
  }
}

function shutdown(code = 0) {
  if (shuttingDown) {
    return;
  }

  shuttingDown = true;
  for (const child of children) {
    stop(child);
  }

  exitTimer = setTimeout(() => process.exit(code), 1000);
  exitTimer.unref();
}

process.on("SIGINT", () => shutdown(0));
process.on("SIGTERM", () => shutdown(0));

if (!isWindows) {
  process.on("SIGHUP", () => shutdown(0));
}

start("Scala.js watcher", isWindows ? "sbt.bat" : "sbt", ["~fastLinkJS"]);
start("Vite", isWindows ? "vite.cmd" : "vite", viteArgs);
