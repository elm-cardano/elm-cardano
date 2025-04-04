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

// Extract the targets from the config
const targets = JSON.parse(fs.readFileSync(targetsConfigPath, "utf8"));

// Create install.js script
const installJs = `const { execSync } = require('child_process');
const os = require('os');
const fs = require('fs');
const path = require('path');
const https = require('https');

const currentPlatform = os.platform();
const currentArch = os.arch();
const targets = ${JSON.stringify(targets)};
const target = targets.find(entry => entry.platform === currentPlatform && entry.arch === currentArch);

if (!target) {
  console.error("Unsupported platform or architecture: " + currentPlatform + "-" + currentArch);
  process.exit(1);
}

const repoUrl = process.env.GITHUB_REPOSITORY || 'your-username/your-repo';
const binaryName = target.asset_name;
const url = "https://github.com/"+ repoUrl +"/releases/download/v${version}/" + binaryName;
const binaryPath = path.join(__dirname, 'binary' + (currentPlatform === 'win32' ? '.exe' : ''));

console.log(\`Downloading \${url}...\`);

// Helper function to handle requests with redirect support
function download(url, dest, callback) {
  const file = fs.createWriteStream(dest);

  const handleError = (err) => {
    fs.unlink(dest, () => {});
    callback(err);
  };

  const handleResponse = (response) => {
    // Handle redirect
    if (response.statusCode >= 300 && response.statusCode < 400 && response.headers.location) {
      file.close();
      console.log(\`Redirected to: \${response.headers.location}\`);
      return download(response.headers.location, dest, callback);
    }

    // Handle error status
    if (response.statusCode !== 200) {
      file.close();
      handleError(new Error(\`Server returned status code \${response.statusCode}\`));
      return;
    }

    // Download the file
    response.pipe(file);

    file.on('finish', () => {
      file.close();
      callback(null);
    });
  };

  https.get(url, handleResponse)
    .on('error', handleError);
}

// Download and make executable
download(url, binaryPath, (err) => {
  if (err) {
    console.error('Failed to download binary:', err.message);
    process.exit(1);
  } else {
    if (currentPlatform !== 'win32') {
      fs.chmodSync(binaryPath, '755');
    }
    console.log('Binary downloaded and made executable');
  }
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
