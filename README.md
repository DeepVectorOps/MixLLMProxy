# LLMHouse

LLM proxy with observability. Matches the `model` field in chat completion requests against configured aliases and proxies to the matching backend.

## Quick start

```
nix-shell --run "cabal run"
```

## Usage

```
curl -X POST http://localhost:8015/api/openai/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{"model":"my-alias","messages":[{"role":"user","content":"hello"}]}'
```

The `model` field is matched against aliases configured at `/ui/aliases`. Each alias has a name, endpoint URL, API key, and model. No match returns 400.

## UI

- `/ui/` — Request log with stats
- `/ui/aliases` — Manage aliases
- `/ui/aliases/info` — Alias instructions

## Env vars

| Variable | Purpose |
|---|---|
| `DB_HOST` / `DB_NAME` / `DB_PORT` / `DB_USER` / `DB_PASSWORD` | PostgreSQL connection |
| `LLM_PORT` | Server port (default 8015) |
