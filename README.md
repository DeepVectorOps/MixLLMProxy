# MixLLMProxy

Single entry point for all your LLM providers. One endpoint, many backends — switch models without changing your integration.

A lightweight, self-hosted alternative to [LiteLLM](https://github.com/BerriAI/litellm).

## Why

Teams using multiple LLM APIs end up juggling keys, endpoints, and model names across every service. MixLLMProxy consolidates them behind one URL. You call one proxy, it routes to the right provider based on the model name you pick.

## What you get

- **Unified endpoint** — your apps call `/api/openai/v1/chat/completions` regardless of which provider is behind it
- **Alias routing** — map friendly names (e.g. `prod-gpt4`) to any downstream endpoint, model, and API key
- **Rate limits** — cap daily requests or tokens per alias; requests exceeding the limit are rejected
- **Observability** — every request logged with latency, tokens, status, and response body; inspectable via built-in web UI
- **No vendor lock-in** — swap backends by changing an alias; zero code changes

## Getting started

```
nix-shell --run "cabal run"
```

Send a request using the alias name as the model:

```
curl -s http://localhost:8015/api/openai/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{"model":"my-alias","messages":[...]}'
```

Manage aliases and set rate limits at `/ui/aliases`. Monitor usage at `/ui/`.

## Requirements

PostgreSQL with the following environment variables:

| Variable | Purpose |
|---|---|
| `DB_HOST` | Postgres host |
| `DB_NAME` | Database name |
| `DB_USER` | Postgres user |
| `DB_PASSWORD` | Postgres password |
| `DB_PORT` | Postgres port |
| `LLM_PORT` | Server port (default 8015) |
