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

CREATE TABLE transactions (
    id SERIAL PRIMARY KEY,
    csg_id TEXT REFERENCES csgs(id),
    participant_id TEXT NOT NULL,
    amount BIGINT NOT NULL,
    transaction_type TEXT NOT NULL,
    timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_transactions_csg_id ON transactions(csg_id);
CREATE INDEX idx_transactions_participant_id ON transactions(participant_id);

CREATE TABLE rewards (
    id SERIAL PRIMARY KEY,
    csg_id TEXT REFERENCES csgs(id),
    participant_id TEXT NOT NULL,
    amount BIGINT NOT NULL,
    claim_time TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_rewards_csg_id ON rewards(csg_id);
CREATE INDEX idx_rewards_participant_id ON rewards(participant_id);
