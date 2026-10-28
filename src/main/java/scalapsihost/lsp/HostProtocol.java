package scalapsihost.lsp;

import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest;
import org.eclipse.lsp4j.services.LanguageClient;
import org.eclipse.lsp4j.services.LanguageServer;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * The server's messages beyond LSP, for editors to show whether it works: its state as it changes (a notification)
 * and a status report on request. Clients that do not know them ignore the notification.
 */
public final class HostProtocol {
  private HostProtocol() {}

  /** The server's state: {@code starting}, {@code importing}, {@code ready} or {@code failed}, and what it is doing. */
  public static final class ServerState {
    public final String state;
    public final String message;

    public ServerState(String state, String message) {
      this.state = state;
      this.message = message;
    }
  }

  /** What the server sends a client that knows its messages. */
  public interface Client extends LanguageClient {
    @JsonNotification("scalaPsiHost/state")
    void state(ServerState state);
  }

  /** What the server answers beyond LSP. */
  public interface Server {
    /** The server's state, project, memory, request latencies, and the problems it logged (see ScalaLanguageServer). */
    @JsonRequest("scalaPsiHost/status")
    CompletableFuture<Map<String, Object>> status();
  }

  /** The server as a client calls it. */
  public interface LanguageServerWithStatus extends LanguageServer, Server {}
}
