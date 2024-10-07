CREATE TABLE csgs (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    participants JSONB DEFAULT '[]',
    cycle_number INTEGER DEFAULT 1,
    week INTEGER DEFAULT 1,
    total_amount BIGINT DEFAULT 0,
    reward_pool BIGINT DEFAULT 0,
    active BOOLEAN DEFAULT TRUE,
    max_participants INTEGER NOT NULL,
    contribution_amount BIGINT NOT NULL,
    start_time TIMESTAMP WITH TIME ZONE NOT NULL,
    end_time TIMESTAMP WITH TIME ZONE NOT NULL
);

CREATE INDEX idx_csgs_active ON csgs(active);
CREATE INDEX idx_csgs_end_time ON csgs(end_time);
