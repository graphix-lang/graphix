import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('graphix');
    const serverPath = config.get<string>('server.path', 'graphix');

    const serverOptions: ServerOptions = {
        run: { command: serverPath, args: ['lsp'] },
        debug: { command: serverPath, args: ['lsp'] },
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'graphix' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.gx'),
        },
    };

    client = new LanguageClient(
        'graphix',
        'Graphix Language Server',
        serverOptions,
        clientOptions
    );

    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
