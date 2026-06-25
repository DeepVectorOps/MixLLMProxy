# MixLLMProxy

OpenAI-compatible LLM proxy with a built-in observability UI. One endpoint, many backends — route by alias name without changing client code.

A lightweight, self-hosted alternative to [LiteLLM](https://github.com/BerriAI/litellm).

## Why

Teams using multiple LLM APIs juggle keys, endpoints, and model names across every service. MixLLMProxy puts them behind a single URL. Clients send `model: "my-alias"`; the proxy forwards to the configured downstream endpoint with the right key and model.

## For LLM API development

When you're building an app that calls chat completions, most of the pain is invisible: prompts go out, responses come back, and you're left grepping logs or re-running requests to figure out what happened.

Point your client at `http://localhost:8015/api/openai/v1/chat/completions` instead of a provider URL. You keep using the normal OpenAI SDK shape — same request body, same response format — but every call is recorded in the UI with full request/response bodies, latency, token counts, and HTTP status.

That makes day-to-day dev work much easier:

- **Debug prompts** — search request/response bodies, open any call on `/ui/request/:id`, inspect JSON without adding logging to your app
- **Watch cost and performance** — see token usage and latency per call; filter to the last 10 minutes while iterating
- **Try models without code changes** — add or switch aliases in the UI (`gpt-4o` today, a cheap local model tomorrow); your app always sends the same alias name
- **Compare providers** — run the same prompt against two aliases and diff the logged responses side by side
- **Test limits safely** — set per-alias daily caps or a global req/s throttle to reproduce rate-limit behavior before production
- **Pause traffic** — hit global pause when you want to stop burn while refactoring client code
- **Catch runaway spend** — live charts and 24h usage bars surface request explosions early (retry loops, bad deploys, runaway agents) before they turn into a surprise bill

Your app only needs one base URL and stable alias names. Keys and downstream endpoints live in the proxy config, not scattered across env files and branches.

## Features

- **Unified API** — `POST /api/openai/v1/chat/completions` (OpenAI-compatible)
- **Alias routing** — map friendly names to endpoint URL, API key, and downstream model
- **Per-alias rate limits** — optional rolling 24h caps on requests and tokens; over-limit requests get 429
- **Global controls** — pause all traffic (503) or enforce a global requests/sec limit from the dashboard
- **Full request logging** — every proxy call stored in PostgreSQL with latency, tokens, status, and bodies
- **Web UI** (`/ui/`) — searchable/sortable request log, per-alias usage bars, live request charts (last 10 min), request detail with JSON tree view
- **Alias management** (`/ui/aliases`) — create, edit, duplicate, delete aliases

## Quick start

### 1. Database

Create a Postgres database and apply the schema:

```bash
psql "$DATABASE_URL" -f db/schema.sql
```

### 2. Environment

Required variables:

| Variable | Purpose |
|---|---|
| `DB_HOST` | Postgres host |
| `DB_PORT` | Postgres port |
| `DB_NAME` | Database name |
| `DB_USER` | Postgres user |
| `DB_PASSWORD` | Postgres password |
| `LLM_PORT` | HTTP port (default `8015`) |

Example (see `etb-llmhouse-env.sh` for a local template):

```bash
export DB_HOST=127.0.0.1
export DB_PORT=5432
export DB_NAME=llmhouse
export DB_USER=postgres
export DB_PASSWORD=...
export LLM_PORT=8015
```

### 3. Run

```bash
nix-shell --run "cabal run"
```

Open the dashboard at [http://localhost:8015/ui/](http://localhost:8015/ui/). Create aliases at `/ui/aliases`.

### 4. Send a request

Use the alias **name** as the `model` field:

```bash
curl -s http://localhost:8015/api/openai/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{"model":"my-alias","messages":[{"role":"user","content":"hello"}]}'
```

No matching alias returns 400. Rate-limited aliases return 429.

## Web UI

| URL | Description |
|---|---|
| `/ui/` | Request log, per-alias 24h rate-limit cards, live charts, global pause/speed controls |
| `/ui/request/:id` | Single-request detail with formatted JSON bodies |
| `/ui/aliases` | Manage aliases and daily limits |
| `/ui/aliases/info` | How aliases work + curl example |

The dashboard supports search (model, alias, bodies, status, …), time filters (`10m`, `1h`, `24h`, `7d`), column sorting, and pagination. Per-alias charts poll `/ui/api/alias-charts` every 5s.

## API

| Endpoint | Method | Description |
|---|---|---|
| `/api/openai/v1/chat/completions` | POST | Proxy chat completions |
| `/ui/api/alias-charts` | GET | JSON time-series data for dashboard charts |

## Development

| Command | Purpose |
|---|---|
| `nix-shell` | Enter dev shell |
| `nix-build` | Nix build |
| `nix-shell --run "cabal run"` | Run server |
| `./lint.sh` | Check compilation (use this, not raw `cabal build`) |
| `nix-shell --run 'source etb-llmhouse-env.sh && cabal test spec'` | Run tests (set `DB_*` for integration tests) |

Edit `package.yaml` for dependency changes — `MixLLMProxy.cabal` is generated by hpack.

## How routing works

```
Client → POST /api/openai/v1/chat/completions
              ↓ match model → alias
         MixLLMProxy → downstream endpoint (alias URL + key + model)
              ↓
         PostgreSQL ← log request/response
              ↑
         /ui/ dashboard
```

The `model` field in the request body is matched against alias names. On match, the proxy forwards with the alias's API key and configured downstream model. All requests are logged regardless of outcome.