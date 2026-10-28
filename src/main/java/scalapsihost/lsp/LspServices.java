package scalapsihost.lsp;

import org.eclipse.lsp4j.services.LanguageClientAware;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.services.TextDocumentService;
import org.eclipse.lsp4j.services.WorkspaceService;

/**
 * Java bases of the server's LSP services. A Scala class that implements these interfaces directly gets forwarders
 * for their default methods, which lsp4j then finds as second RPC methods of the same name ("Duplicate RPC method");
 * inherited through a Java class, the default methods stay the interfaces' own.
 */
final class LspServices {
  private LspServices() {}

  abstract static class Server implements LanguageServer, LanguageClientAware, HostProtocol.Server {}

  abstract static class TextDocuments implements TextDocumentService {}

  abstract static class Workspace implements WorkspaceService {}
}
