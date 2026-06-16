# LLMHouse

LLM proxy with observability. Accepts OpenAI-compatible chat completion requests, matches the `model` field against configured aliases, forwards to the aliased downstream LLM endpoint, and logs every request/response to PostgreSQL for inspection via a built-in web UI.

## Endpoints

| Endpoint | Method | Description |
|---|---|---|
| `/api/openai/v1/chat/completions` | POST | Chat completions proxy — matches `model` field to alias |
| `/ui/` | GET | Observatory: table of logged API requests |
| `/ui/aliases` | GET | Manage aliases (CRUD) |
| `/ui/aliases/info` | GET | Alias usage instructions |

## Architecture

```
Client → /api/openai/v1/chat/completions → LLMHouse → alias endpoint (or 400)
                                                 ↓
                                            PostgreSQL (llm_requests, aliases)
                                                 ↑
Client → /ui/ → Lucid HTML ────────────────────┘
```

The proxy reads the `model` field from the incoming request body and looks up a matching alias by name. If found, the request is forwarded to the alias's endpoint with the alias's API key and the alias's configured model (the request body's model is overridden). If no alias matches, a 400 error is returned.

Every request is logged to the `llm_requests` table with request body, response body, status code, latency, model, and alias name.

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
| model | TEXT | Model extracted/overridden |
| prompt_tokens | INT | Prompt token count from response |
| completion_tokens | INT | Completion token count from response |
| total_tokens | INT | Total token count from response |
| alias_name | TEXT | Alias name that matched |
| created_at | TIMESTAMPTZ | Timestamp of the request |

### aliases

| Column | Type | Description |
|---|---|---|
| id | SERIAL | Auto-incrementing primary key |
| name | TEXT | Unique alias name (matched against request `model` field) |
| endpoint_url | TEXT | Downstream LLM endpoint |
| api_key | TEXT | API key sent as Bearer token |
| model | TEXT | Model forwarded to downstream |
| created_at | TIMESTAMPTZ | Timestamp of creation |

## Environment Variables

| Variable | Purpose |
|---|---|
| `DB_HOST` | PostgreSQL host |
| `DB_NAME` | Database name |
| `DB_PASSWORD` | PostgreSQL password |
| `DB_PORT` | PostgreSQL port |
| `DB_USER` | PostgreSQL user |
| `LLM_PORT` | Port to run the server on |

## Development

- **Enter dev shell**: `nix-shell`
- **Build**: `nix-build`
- **Run**: `nix-shell --run "cabal run"`
- **Check compilation**: `./lint.sh`
