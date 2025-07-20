# Thrift Crowd Staking - Technical Documentation

## Overview
Thrift Crowd Staking is a decentralized Community Savings Groups (CSG) platform built on Cardano. It enables trustless, permissionless savings groups through blockchain technology and real Cardano staking rewards.

## Architecture

### Core Components
1. **Smart Contracts** (Plutus) - On-chain validation and fund management
2. **Backend API** (Haskell) - Cardano node interface and data indexing  
3. **Frontend** (Next.js) - User interface with Lace wallet integration

### Key Features
- **Wallet-Only Authentication** - No KYC, no personal data collection
- **Real Cardano Staking** - Actual delegation to stake pools with real rewards
- **Load-Balanced Pool Selection** - Distributes CSGs across multiple pools
- **Smart Contract Enforcement** - All rules enforced on-chain

## Smart Contracts

**Location**: `backend/src/CSGContract.hs`

### Core Functions
- `validateCSG` - Main validator for CSG operations
- `validateJoin` - Validates participant joining
- `validateClaimReward` - Validates reward claims
- `scrAddress` - Contract script address

## Backend API

**Framework**: Servant (Haskell)  
**Database**: PostgreSQL  
**Key Files**:
- `backend/src/Main.hs` - Application entry point
- `backend/src/API.hs` - REST API endpoints  
- `backend/src/CSG.hs` - CSG business logic
- `backend/src/Cardano.hs` - Blockchain integration

### API Endpoints
```
GET  /api/health          - Health check
POST /api/create-csg      - Create new CSG
POST /api/join-csg/{id}   - Join existing CSG
POST /api/claim-reward/{id} - Claim staking rewards
POST /api/close-csg/{id}  - Close CSG
GET  /api/list-csgs       - List active CSGs
POST /api/withdraw/{id}   - Withdraw funds
```

### Cardano Integration
- **Blockfrost API** - Pool data and reward queries
- **cardano-cli** - Transaction building and submission
- **Real UTXO Management** - Queries and selects actual UTXOs
- **Pool Selection Algorithm** - ROA-based scoring with load balancing

## Frontend

**Framework**: Next.js 14 (React)  
**Wallet**: Lace integration  
**UI**: Tailwind CSS + shadcn/ui  

### Key Pages
- `app/page.tsx` - Home/dashboard
- `app/create/page.tsx` - Create CSG form
- `app/group/[id]/page.tsx` - CSG details
- `app/search/page.tsx` - Browse CSGs

### Wallet Integration
- `components/wallet-provider.tsx` - Lace wallet connection
- Direct transaction signing through wallet
- No backend authentication required

## Development

### Prerequisites
- Docker & Docker Compose
- Node.js 18+ (for local frontend development)
- Git

### Quick Start
```bash
git clone https://github.com/Josiah-O/thrift-crowd-staking
cd thrift-crowd-staking
docker-compose up --build
```

### Environment Variables
```bash
# Backend
DATABASE_URL=postgresql://postgres:password@db:5432/thrift_crowd_staking
BLOCKFROST_API_KEY=your_blockfrost_key
CARDANO_NETWORK=preprod

# Frontend  
NEXT_PUBLIC_API_URL=http://localhost:8080
```

## Deployment

### Docker Production
```bash
docker-compose -f docker-compose.yml up -d
```

### Environment Setup
- **Testnet**: Use `preprod` network for testing
- **Mainnet**: Configure production Blockfrost keys
- **Database**: PostgreSQL 13+
- **Cardano Node**: Synced node for transaction submission

## Database Schema

### Core Tables
- `csgs` - CSG metadata and status
- `csg_participants` - Participant stakes and addresses
- `csg_transactions` - Transaction history
- `csg_rewards` - Reward distribution records

## Security

### DApp Security Model
- **No Backend Authentication** - Pure wallet-based access
- **Smart Contract Validation** - All rules enforced on-chain
- **Real Wallet Signatures** - Users sign transactions directly
- **No Personal Data** - Only wallet addresses stored

### Operational Security
- Environment variable management
- HTTPS enforcement
- Rate limiting on API endpoints
- Input validation and sanitization

## Monitoring

### Key Metrics
- CSG creation/participation rates
- Staking delegation success rates
- Transaction success/failure rates
- Pool performance tracking

### Logging
- Structured logging with correlation IDs
- Error tracking and alerting
- Performance monitoring

## Contributing

### Development Workflow
1. Fork repository
2. Create feature branch
3. Implement changes with tests
4. Submit pull request
5. Code review and merge

### Code Standards
- Haskell: HLint compliance
- TypeScript: ESLint + Prettier
- Documentation: Keep technical docs updated
- Testing: Unit tests for core functions

## License

BSD-3-Clause - See LICENSE file for details
