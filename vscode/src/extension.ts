// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import path = require('path');
import fs = require('fs');
import fgr = require('@terascope/fetch-github-release');

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	//  Download the kernel into extension's directory and write the configuration into
	//  `jsonPath` JSON file. Open it for the user.
	let installKernel = async (jsonPath: string) => {
		const user = 'reznikmm';
		const repo = 'jupyter';
		const outputdir = context.extensionPath;
		const leaveZipped = false;
		const disableLogging = false;
		const filter = (asset: { name: string }): boolean =>
			 asset.name.includes(process.platform + '-'+ process.arch);

		fgr.downloadRelease(user, repo, outputdir, undefined, filter, leaveZipped, disableLogging)
			.then(async () => {
				fs.readFile(path.join(outputdir, path.basename(jsonPath)), 'utf8', async (err, data) => {
					if (!err) {
						let dir = path.dirname(jsonPath);
						let object = JSON.parse(data);
						object.argv[0] = path.join(outputdir, 'ada_kernel');
						if (!fs.existsSync(dir)) { fs.mkdirSync(dir, {recursive: true}); };
						fs.writeFileSync(jsonPath, JSON.stringify(object, null, 4));
						let doc = await vscode.workspace.openTextDocument(jsonPath);
						await vscode.window.showTextDocument(doc, { preview: false });
						vscode.window.showInformationMessage('The Ada Kernel has been installed!');
					}
				});
			})
			.catch(function (err) {
				vscode.window.showErrorMessage(err.message);
			});
	};

	// The command has been defined in the package.json file
	// Now provide the implementation of the command with registerCommand
	// The commandId parameter must match the command field in package.json
	let disposable = vscode.commands.registerCommand('jupyter-ada-kernel.install', () => {
		// Directory where Jupyter searches for configurations
		let jupyterPath = process.env['JUPYTER_PATH'] ||
			path.join(process.env['HOME'] || '/', '.local', 'share', 'jupyter');

		// Ada kernel configuration
		let kernelJsonPath = path.join(jupyterPath, 'kernels', 'ada', 'kernel.json');

		//  Try to read Ada kernel configuration
		fs.readFile(kernelJsonPath, 'utf8', (err, data) => {
			var success = false;
			if (!err) {
				let object = JSON.parse(data);
				success = fs.existsSync(object.argv[0] || '/#no-such-file');
			}
			if (success) {
				vscode.window.showInformationMessage('The Ada Kernel is in place, no installation required.');
			} else {
				installKernel(kernelJsonPath);
			};
		});
	});

	context.subscriptions.push(disposable);
}

// This method is called when your extension is deactivated
export function deactivate() { }
