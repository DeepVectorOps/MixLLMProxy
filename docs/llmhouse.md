# MixLLMProxy

OpenAI-compatible LLM proxy with observability. Matches the `model` field to configured aliases, forwards to downstream endpoints, and logs every request/response to PostgreSQL. Built-in web UI for log inspection, rate-limit monitoring, and alias management.

## Endpoints

| Endpoint | Method | Description |
|---|---|---|
| `/api/openai/v1/chat/completions` | POST | Chat completions proxy — matches `model` to alias |
| `/ui/` | GET | Request log with per-alias rate limits (rolling 24h) |
| `/ui/aliases` | GET | Alias CRUD |
| `/ui/aliases/info` | GET | Alias usage guide |

## Architecture

```
Client → /api/openai/v1/chat/completions → MixLLMProxy → alias endpoint (or 400)
                                                    ↓
                                               PostgreSQL
                                                    ↑
Client → /ui/ — Lucid HTML ───────────────────────┘
```

The `model` field in the request body is matched against alias names. On match, the request is forwarded with the alias's API key and configured model (overriding the request's model field). No match returns 400. All requests are logged with body, status, latency, tokens, and alias name.

## Database

### llm_requests

| Column | Type | Description |
|---|---|---|
| id | SERIAL | Primary key |
| endpoint | TEXT | Requested API path |
| method | TEXT | HTTP method |
| request_body | TEXT | Raw request body |
| response_status | INT | Downstream HTTP status |
| response_body | TEXT | Raw response body |
| latency_ms | DOUBLE PRECISION | Round-trip latency (ms) |
| model | TEXT | Model sent downstream |
| prompt_tokens | INT | Prompt tokens from response |
| completion_tokens | INT | Completion tokens from response |
| total_tokens | INT | Total tokens from response |
| alias_name | TEXT | Matched alias |
| created_at | TIMESTAMPTZ | Request timestamp |

### aliases

| Column | Type | Description |
|---|---|---|
| id | SERIAL | Primary key |
| name | TEXT | Unique alias name (matched against `model`) |
| endpoint_url | TEXT | Downstream LLM endpoint |
| api_key | TEXT | Bearer token sent downstream |
| model | TEXT | Model sent downstream |
| daily_token_limit | INT | Optional rolling 24h token limit (null = ∞) |
| daily_request_limit | INT | Optional rolling 24h request limit (null = ∞) |
| created_at | TIMESTAMPTZ | Creation timestamp |

## Environment

| Variable | Purpose |
|---|---|
| `DB_HOST` / `DB_NAME` / `DB_PASSWORD` / `DB_PORT` / `DB_USER` | PostgreSQL connection |
| `LLM_PORT` | Server port (default 8015) |

## Development

| Command | Purpose |
|---|---|
| `nix-shell` | Enter dev shell |
| `nix-build` | Build |
| `nix-shell --run "cabal run"` | Run |
| `./lint.sh` | Check compilation |
