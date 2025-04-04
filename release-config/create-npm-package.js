// Create an NPM binary package for multiple platforms and architectures

const fs = require("fs");
const path = require("path");

// Retrieve arguments from the command line
const [, , packageJsonFilePath, version, targetsConfigPath] = process.argv;

// Create the folder npm-package/npm/
const npmPackageDir = path.join(process.cwd(), "npm-package");
const npmDir = path.join(npmPackageDir, "npm");
fs.mkdirSync(npmDir, { recursive: true });

// Create package.json
// Starts with the user-provided one, and adds sections for the binary
const packageJson = JSON.parse(fs.readFileSync(packageJsonFilePath, "utf8"));
packageJson["version"] = version;
packageJson["bin"] = {};
packageJson["bin"][packageJson.name] = "npm/run.js";
packageJson["scripts"] = {};
packageJson["scripts"]["postinstall"] = "node npm/install.js";
packageJson["files"] = ["npm/**/*"];

fs.writeFileSync(
    path.join(npmPackageDir, "package.json"),
    JSON.stringify(packageJson, null, 2),
);

// Create install.js script
const installJs = `const { execSync } = require('child_process');
const os = require('os');
const fs = require('fs');
const path = require('path');
const https = require('https');

const currentPlatform = os.platform(); // e.g., 'darwin', 'linux', 'win32'
const currentArch = os.arch(); // e.g., 'x64', 'arm64'
const targets = JSON.parse(fs.readFileSync(${targetsConfigPath}, "utf8"));
const target = targets.find(entry => entry.platform === currentPlatform && entry.arch === currentArch);
if (!target) {
  throw new Error("Unsupported platform or architecture: " + currentPlatform + "-" + currentArch);
}

const repoUrl = process.env.GITHUB_REPOSITORY || 'your-username/your-repo';
const binaryName = target.asset_name;
const url = "https://github.com/"+ repoUrl +"/releases/download/v${version}/" + binaryName;
const binaryPath = path.join(__dirname, 'binary' + (platform === 'win32' ? '.exe' : ''));

console.log(\`Downloading \${url}...\`);

const file = fs.createWriteStream(binaryPath);
https.get(url, function(response) {
  response.pipe(file);
  file.on('finish', function() {
    file.close();
    if (platform !== 'win32') {
      fs.chmodSync(binaryPath, '755');
    }
    console.log('Binary downloaded and made executable');
  });
}).on('error', function(err) {
  fs.unlink(binaryPath);
  console.error('Failed to download binary:', err.message);
});`;

fs.writeFileSync(path.join(npmDir, "install.js"), installJs);

// Create run.js script
const runJs = `#!/usr/bin/env node
const { spawn } = require('child_process');
const path = require('path');
const os = require('os');

const binary = spawn(
  path.join(__dirname, 'binary' + (os.platform() === 'win32' ? '.exe' : '')),
  process.argv.slice(2),
  { stdio: 'inherit' }
);

binary.on('close', code => {
  process.exit(code);
});`;

fs.writeFileSync(path.join(npmDir, "run.js"), runJs);
fs.chmodSync(path.join(npmDir, "run.js"), "755");

console.log("NPM package structure created successfully");
