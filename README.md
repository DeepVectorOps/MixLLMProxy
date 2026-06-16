# MixLLMProxy

Route requests to any LLM backend by model name. Configure an alias (name → endpoint + API key + model) and use that name in your `chat/completions` calls. Every request and response is logged to Postgres for inspection.

Under 1000 lines of Haskell. No frameworks, no bloat.

## Quick start

```
nix-shell --run "cabal run"
```

## Usage

```
curl -s http://localhost:8015/api/openai/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{"model":"my-alias","messages":[{"role":"user","content":"hello"}]}'
```

Create aliases at `/ui/aliases` — give them a name, a downstream endpoint URL, an API key, and a model. No matching alias returns 400.

## UI

| Path | What |
|---|---|
| `/ui/` | Request log, latency, token stats |
| `/ui/aliases` | Create/edit/delete aliases |
| `/ui/aliases/info` | How aliases work |

## Env vars

| Variable | Purpose |
|---|---|
| `DB_HOST` / `DB_NAME` / `DB_PORT` / `DB_USER` / `DB_PASSWORD` | Postgres connection |
| `LLM_PORT` | Server port (default 8015) |
