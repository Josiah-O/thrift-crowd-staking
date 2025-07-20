# Cardano Configuration

## Environment Setup

### Required Environment Variables

```bash
# Backend Configuration
DATABASE_URL=postgresql://postgres:password@db:5432/thrift_crowd_staking
BLOCKFROST_API_KEY=your_blockfrost_api_key
CARDANO_NETWORK=preprod

# Frontend Configuration  
NEXT_PUBLIC_API_URL=http://localhost:8080
```

### Blockfrost API Setup

1. **Create Account**: [https://blockfrost.io](https://blockfrost.io)
2. **Generate API Key**: Create a **preprod** project for testing
3. **Set Environment Variable**:
   ```bash
   export BLOCKFROST_API_KEY=preprodABC123XYZ789
   ```

### Cardano Network Configuration

The application supports multiple networks:
- **preprod** - Testnet environment (recommended for development)
- **mainnet** - Production environment

Set via environment variable:
```bash
export CARDANO_NETWORK=preprod
```

## Wallet Integration

### Lace Wallet Support
- Users connect directly through Lace browser extension
- No backend wallet management required
- Transactions signed client-side through wallet

### Transaction Types

The application creates these transaction types:
- **CSG Creation** - Initialize new savings group contract
- **Join CSG** - Participate in existing savings group
- **Claim Rewards** - Withdraw accumulated staking rewards
- **Close CSG** - End savings group and return funds
- **Withdraw** - Early withdrawal from savings group

## Security

### Environment Variables
- **Never commit secrets** to version control
- Use `.env` files for local development
- Set environment variables in production deployment

### Production Deployment
- **GitHub Actions**: Use repository secrets
- **Cloud Providers**: Use secure parameter stores
- **Docker**: Use Docker secrets or environment injection

## Development Setup

### Docker Compose
```bash
# Set environment variables
export BLOCKFROST_API_KEY=your_key
export CARDANO_NETWORK=preprod

# Start services
docker-compose up --build
```

### Local Development
```bash
# Backend
cd backend
export DATABASE_URL=postgresql://...
export BLOCKFROST_API_KEY=...
stack run

# Frontend
cd frontend
npm install
npm run dev
```

## Cardano Node Integration

### Transaction Building
- Uses `cardano-cli` for transaction construction
- Queries UTXOs via Blockfrost API
- Submits transactions to Cardano network

### Stake Pool Selection
- Real-time pool performance queries
- ROA-based scoring algorithm
- Load balancing across multiple pools

## Monitoring

### Key Metrics
- Transaction success rates
- Pool performance tracking
- Reward distribution accuracy
- Network sync status

### Logging
- Structured transaction logging
- Error tracking and alerting
- Performance monitoring 