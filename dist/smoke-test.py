#!/usr/bin/env python3
"""Smoke test of a built distribution: starts the language server as an editor would and talks LSP to it.

Usage: smoke-test.py <bin/scala-psi-host> <project dir> <source file> <line> <column>

Initializes on the project, opens the source file, waits for its diagnostics, asks for hover and definition at
<line>:<column> (0-based), shuts the server down, and prints what it got. Exits non-zero when the server does not
answer, answers with an error, or exits with an error.
"""
import json
import pathlib
import subprocess
import sys
import threading
import time

TIMEOUT_SECONDS = 600  # opening a build over BSP starts the build server and can take minutes the first time


def main():
    server, project, source, line, column = sys.argv[1], pathlib.Path(sys.argv[2]).resolve(), pathlib.Path(sys.argv[3]).resolve(), int(sys.argv[4]), int(sys.argv[5])
    process = subprocess.Popen([server], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    stdin, stdout = process.stdin, process.stdout
    assert stdin is not None and stdout is not None
    messages, lock = [], threading.Condition()

    def read():
        while True:
            headers = {}
            while True:
                header = stdout.readline()
                if not header:
                    return
                header = header.decode().strip()
                if not header:
                    break
                name, value = header.split(":", 1)
                headers[name.strip().lower()] = value.strip()
            body = json.loads(stdout.read(int(headers["content-length"])))
            with lock:
                messages.append(body)
                lock.notify_all()

    threading.Thread(target=read, daemon=True).start()
    next_id = [0]

    def send(method, params, request=True):
        message = {"jsonrpc": "2.0", "method": method, "params": params}
        if request:
            next_id[0] += 1
            message["id"] = next_id[0]
        data = json.dumps(message).encode()
        stdin.write(b"Content-Length: %d\r\n\r\n" % len(data) + data)
        stdin.flush()
        return message.get("id")

    def wait_for(matches, what):
        deadline = time.time() + TIMEOUT_SECONDS
        with lock:
            while True:
                found = next((m for m in messages if matches(m)), None)
                if found:
                    return found
                if time.time() > deadline or process.poll() is not None:
                    sys.exit(f"no {what} (server exit code: {process.poll()})")
                lock.wait(timeout=1)

    def request(method, params):
        id = send(method, params)
        response = wait_for(lambda m: m.get("id") == id, f"answer to {method}")
        if "error" in response:
            sys.exit(f"{method} failed: {response['error']}")
        return response["result"]

    uri, document = source.as_uri(), {"uri": source.as_uri()}
    position = {"line": line, "character": column}
    request("initialize", {"processId": None, "rootUri": project.as_uri(), "capabilities": {},
                           "workspaceFolders": [{"uri": project.as_uri(), "name": project.name}]})
    send("initialized", {}, request=False)
    started = time.time()
    send("textDocument/didOpen", {"textDocument": {"uri": uri, "languageId": "scala", "version": 1, "text": source.read_text()}}, request=False)
    diagnostics = wait_for(lambda m: m.get("method") == "textDocument/publishDiagnostics" and m["params"]["uri"] == uri, "diagnostics")
    print(f"diagnostics after {time.time() - started:.1f} s (including opening the project):")
    for d in diagnostics["params"]["diagnostics"]:
        print(f"  {d['range']['start']['line']}:{d['range']['start']['character']} [{d.get('source')}] {d['message']}")
    print("hover:", json.dumps(request("textDocument/hover", {"textDocument": document, "position": position})))
    print("definition:", json.dumps(request("textDocument/definition", {"textDocument": document, "position": position})))
    request("shutdown", None)
    send("exit", None, request=False)
    stdin.close()
    if process.wait(timeout=60) != 0:
        sys.exit(f"the server exited with code {process.returncode}")


if __name__ == "__main__":
    main()
