CREATE TABLE IF NOT EXISTS llm_requests (
    id SERIAL PRIMARY KEY,
    endpoint TEXT NOT NULL,
    method TEXT NOT NULL,
    request_body TEXT,
    response_status INT,
    response_body TEXT,
    latency_ms DOUBLE PRECISION,
    model TEXT,
    prompt_tokens INT,
    completion_tokens INT,
    total_tokens INT,
    alias_name TEXT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_llm_requests_created_at ON llm_requests(created_at DESC);

CREATE TABLE IF NOT EXISTS aliases (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    endpoint_url TEXT NOT NULL,
    api_key TEXT NOT NULL,
    model TEXT NOT NULL,
    daily_token_limit INT,
    daily_request_limit INT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

ALTER TABLE aliases ADD COLUMN IF NOT EXISTS daily_token_limit INT;
ALTER TABLE aliases ADD COLUMN IF NOT EXISTS daily_request_limit INT;
