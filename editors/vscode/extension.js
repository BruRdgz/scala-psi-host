// Starts the scala-psi-host language server for Scala files. The server opens the workspace's build over BSP:
// the workspace needs a BSP connection file (.bsp/, e.g. from `sbt bspConfig`).
const vscode = require('vscode');
const { LanguageClient } = require('vscode-languageclient/node');

let client;

async function activate(context) {
  const config = vscode.workspace.getConfiguration('scalaPsiHost');
  const serverPath = config.get('serverPath');
  if (!serverPath) {
    vscode.window.showErrorMessage('scala-psi-host: set "scalaPsiHost.serverPath" to bin/scala-psi-host of a built distribution.');
    return;
  }
  const javaHome = config.get('javaHome');
  const env = javaHome ? { ...process.env, JAVA_HOME: javaHome } : process.env;
  client = new LanguageClient(
    'scalaPsiHost',
    'scala-psi-host',
    { command: serverPath, options: { env } },
    { documentSelector: [{ scheme: 'file', language: 'scala' }] },
  );
  context.subscriptions.push(client);
  await client.start();
}

function deactivate() {
  return client ? client.stop() : undefined;
}

module.exports = { activate, deactivate };
