import { existsSync, readdirSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { spawnSync } from "node:child_process";

const isWindows = process.platform === "win32";
const javaExe = isWindows ? "java.exe" : "java";

function expandHome(path) {
  if (!path || !path.startsWith("~")) {
    return path;
  }

  return join(process.env.HOME || process.env.USERPROFILE || "", path.slice(1));
}

function javaPath(javaHome) {
  return join(javaHome, "bin", javaExe);
}

function hasJava(javaHome) {
  return !!javaHome && existsSync(javaPath(javaHome));
}

function parseMajorVersion(versionText) {
  const match = versionText.match(/version "([^"]+)"/);
  if (!match) {
    return undefined;
  }

  const [head, tail] = match[1].split(".");
  if (head === "1" && tail) {
    return Number.parseInt(tail, 10);
  }

  return Number.parseInt(head, 10);
}

function javaVersion(javaHome) {
  const result = spawnSync(javaPath(javaHome), ["-version"], {
    encoding: "utf8",
  });
  const text = `${result.stdout || ""}${result.stderr || ""}`;
  const major = parseMajorVersion(text);

  if (!major) {
    return undefined;
  }

  return { major, text: text.trim() };
}

function addIfJava(candidates, javaHome) {
  const normalized = javaHome && resolve(expandHome(javaHome));
  if (hasJava(normalized)) {
    candidates.add(normalized);
  }
}

function addChildren(candidates, parent, predicate = () => true) {
  const normalizedParent = expandHome(parent);
  if (!normalizedParent || !existsSync(normalizedParent)) {
    return;
  }

  for (const entry of readdirSync(normalizedParent, { withFileTypes: true })) {
    if (!entry.isDirectory() || !predicate(entry.name)) {
      continue;
    }

    addIfJava(candidates, join(normalizedParent, entry.name));
    addIfJava(candidates, join(normalizedParent, entry.name, "Contents", "Home"));
    addIfJava(candidates, join(normalizedParent, entry.name, "jbr"));
  }
}

function addPathJava(candidates) {
  const command = isWindows ? "where" : "which";
  const result = spawnSync(command, ["java"], { encoding: "utf8" });
  if (result.status !== 0) {
    return;
  }

  for (const line of result.stdout.split(/\r?\n/)) {
    const bin = line.trim();
    if (bin) {
      addIfJava(candidates, dirname(dirname(bin)));
    }
  }
}

export function resolveJavaHome(minMajor = 17) {
  const candidates = new Set();

  addIfJava(candidates, process.env.JAVA_HOME);
  addPathJava(candidates);
  addChildren(candidates, "~/software/package", name => true);
  addChildren(candidates, "~/.jdks", name => true);
  addChildren(candidates, "/usr/lib/jvm", name => true);
  addChildren(candidates, "/opt", name => /jdk|jbr|java/i.test(name));

  if (isWindows) {
    addChildren(candidates, `${process.env.ProgramFiles || "C:\\Program Files"}\\Java`);
    addChildren(candidates, `${process.env.ProgramFiles || "C:\\Program Files"}\\Eclipse Adoptium`);
    addChildren(candidates, `${process.env["ProgramFiles(x86)"] || "C:\\Program Files (x86)"}\\Java`);
  }

  const matches = [...candidates]
    .map(javaHome => ({ javaHome, version: javaVersion(javaHome) }))
    .filter(candidate => candidate.version && candidate.version.major >= minMajor)
    .sort((a, b) => b.version.major - a.version.major);

  return matches[0]?.javaHome;
}

export function withJavaEnv(baseEnv = process.env, minMajor = 17) {
  const javaHome = resolveJavaHome(minMajor);
  if (!javaHome) {
    throw new Error(
      `Scala 3.8 requires JDK ${minMajor}+; set JAVA_HOME to a JDK ${minMajor}+ installation.`,
    );
  }

  return {
    ...baseEnv,
    JAVA_HOME: javaHome,
    PATH: `${join(javaHome, "bin")}${isWindows ? ";" : ":"}${baseEnv.PATH || ""}`,
  };
}
