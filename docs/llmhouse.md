# LLMHouse

LLM proxy with observability. Accepts OpenAI-compatible chat completion requests, forwards them to a downstream LLM endpoint, and logs every request/response to PostgreSQL for inspection via a built-in web UI.

## Endpoints

| Endpoint | Method | Description |
|---|---|---|
| `/api/openai/v1/chat/completions` | POST | OpenAI-compatible chat completions proxy |
| `/ui/` | GET | Observatory: table of logged API requests |

## Architecture

```
Client → /api/openai/v1/chat/completions → LLMHouse → downstream LLM endpoint
                                                ↓
                                           PostgreSQL (llm_requests)
                                                ↑
Client → /ui/ → Lucid HTML table ──────────────┘
```

The proxy performs raw HTTP forwarding. It reads the incoming request body, attaches the configured API key as an `Authorization: Bearer` header, sends it to the downstream endpoint, and returns the response verbatim. Every request is logged to the `llm_requests` table with request body, response body, status code, latency, and the model extracted from the request JSON.

## Database

### llm_requests

| Column | Type | Description |
|---|---|---|
| id | SERIAL | Auto-incrementing primary key |
| endpoint | TEXT | Requested API path |
| method | TEXT | HTTP method |
| request_body | TEXT | Raw request body |
| response_status | INT | HTTP status code from downstream |
| response_body | TEXT | Raw response body |
| latency_ms | DOUBLE PRECISION | Round-trip latency in milliseconds |
| model | TEXT | Model extracted from request body |
| prompt_tokens | INT | Prompt token count from response |
| completion_tokens | INT | Completion token count from response |
| total_tokens | INT | Total token count from response |
| created_at | TIMESTAMPTZ | Timestamp of the request |

## Environment Variables

| Variable | Purpose |
|---|---|
| `DB_HOST` | PostgreSQL host |
| `DB_NAME` | Database name (default: `llmhouse`) |
| `DB_PASSWORD` | PostgreSQL password |
| `DB_PORT` | PostgreSQL port |
| `DB_USER` | PostgreSQL user |
| `LLM_API_KEY` | Downstream LLM API key (sent as Bearer token) |
| `LLM_API_URL` | Downstream LLM endpoint URL |
| `LLM_MODEL` | Model to forward requests to |
| `LLM_PORT` | Port to run the server on |

## Development

- **Enter dev shell**: `nix-shell`
- **Build**: `nix-build`
- **Run**: `source etb-llmhouse-env.sh && nix-shell --run "cabal run"`
- **Check compilation**: `./lint.sh`
