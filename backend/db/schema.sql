CREATE TABLE IF NOT EXISTS csgs (
    id VARCHAR(64) PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    contract_address VARCHAR(255) NOT NULL,
    stake_amount BIGINT NOT NULL,
    duration INTEGER NOT NULL,
    start_time TIMESTAMPTZ NOT NULL,
    end_time TIMESTAMPTZ NOT NULL,
    status VARCHAR(50) NOT NULL,
    pool_id VARCHAR(64),
    delegation_tx_hash VARCHAR(128),
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS csg_participants (
    id SERIAL PRIMARY KEY,
    csg_id VARCHAR(64) REFERENCES csgs(id),
    participant_address VARCHAR(255) NOT NULL,
    stake_amount BIGINT NOT NULL,
    join_tx_hash VARCHAR(128) NOT NULL,
    joined_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS csg_transactions (
    id SERIAL PRIMARY KEY,
    csg_id VARCHAR(64) REFERENCES csgs(id),
    tx_hash VARCHAR(128) NOT NULL,
    tx_type VARCHAR(50) NOT NULL, -- 'CREATE', 'JOIN', 'CLAIM', 'CLOSE', 'DELEGATE'
    participant_address VARCHAR(255),
    amount BIGINT,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS csg_rewards (
    id SERIAL PRIMARY KEY,
    csg_id VARCHAR(64) REFERENCES csgs(id),
    participant_address VARCHAR(255) NOT NULL,
    reward_amount BIGINT NOT NULL,
    reward_epoch INTEGER NOT NULL,
    claimed BOOLEAN DEFAULT FALSE,
    claim_tx_hash VARCHAR(128),
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_csgs_status ON csgs(status);
CREATE INDEX idx_csgs_pool_id ON csgs(pool_id);
CREATE INDEX idx_csg_participants_csg_id ON csg_participants(csg_id);
CREATE INDEX idx_csg_participants_address ON csg_participants(participant_address);
CREATE INDEX idx_csg_transactions_csg_id ON csg_transactions(csg_id);
CREATE INDEX idx_csg_transactions_tx_hash ON csg_transactions(tx_hash);
CREATE INDEX idx_csg_rewards_csg_id ON csg_rewards(csg_id);
CREATE INDEX idx_csg_rewards_participant ON csg_rewards(participant_address);
CREATE INDEX idx_csg_rewards_claimed ON csg_rewards(claimed);
