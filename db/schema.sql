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
CREATE INDEX IF NOT EXISTS idx_llm_requests_alias_created ON llm_requests(alias_name, created_at DESC);

CREATE TABLE IF NOT EXISTS endpoints (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    url TEXT NOT NULL,
    api_key TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS aliases (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    endpoint_id INT NOT NULL REFERENCES endpoints(id),
    model TEXT NOT NULL,
    daily_token_limit INT,
    daily_request_limit INT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

ALTER TABLE aliases ADD COLUMN IF NOT EXISTS daily_token_limit INT;
ALTER TABLE aliases ADD COLUMN IF NOT EXISTS daily_request_limit INT;
ALTER TABLE aliases ADD COLUMN IF NOT EXISTS endpoint_id INT REFERENCES endpoints(id);

DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_name = 'aliases' AND column_name = 'endpoint_url'
  ) THEN
    INSERT INTO endpoints (name, url, api_key)
    SELECT DISTINCT ON (a.endpoint_url, a.api_key)
      'ep-' || substr(md5(a.endpoint_url || a.api_key), 1, 8),
      a.endpoint_url,
      a.api_key
    FROM aliases a
    WHERE a.endpoint_id IS NULL
      AND NOT EXISTS (
        SELECT 1 FROM endpoints e
        WHERE e.url = a.endpoint_url AND e.api_key = a.api_key
      );

    UPDATE aliases a
    SET endpoint_id = e.id
    FROM endpoints e
    WHERE a.endpoint_url = e.url
      AND a.api_key = e.api_key
      AND a.endpoint_id IS NULL;

    ALTER TABLE aliases DROP COLUMN IF EXISTS endpoint_url;
    ALTER TABLE aliases DROP COLUMN IF EXISTS api_key;
  END IF;
END $$;

ALTER TABLE aliases ALTER COLUMN endpoint_id SET NOT NULL;
