# Thrift Crowd Staking

> Decentralized Community Savings Groups on Cardano

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](LICENSE)
[![Cardano](https://img.shields.io/badge/Built%20on-Cardano-blue)](https://cardano.org/)
[![Haskell](https://img.shields.io/badge/Backend-Haskell-purple)](https://www.haskell.org/)
[![Next.js](https://img.shields.io/badge/Frontend-Next.js-black)](https://nextjs.org/)

Thrift Crowd Staking enables trustless, permissionless Community Savings Groups (CSGs) on the Cardano blockchain. Users can create and participate in savings groups that automatically delegate to stake pools and distribute real staking rewards.

## Features

- **🔐 Wallet-Only Authentication** - No KYC, no personal data, pure DApp
- **💰 Real Cardano Staking** - Actual delegation to stake pools with real rewards
- **⚖️ Load-Balanced Pool Selection** - Distributes CSGs across multiple top pools
- **🔒 Smart Contract Enforcement** - All rules enforced on-chain
- **🌐 Lace Wallet Integration** - Seamless connection and transaction signing
- **📊 Transparent Operations** - All transactions visible on Cardano explorer

## Technology Stack

- **Smart Contracts**: Plutus (integrated)
- **Backend**: Haskell with Servant API
- **Frontend**: Next.js 14 with TypeScript
- **Database**: PostgreSQL
- **Blockchain**: Cardano (preprod/mainnet)
- **Wallet**: Lace integration

## Quick Start

### Prerequisites

- Docker & Docker Compose
- Lace Wallet browser extension
- Blockfrost API key ([get one here](https://blockfrost.io))

### Installation

```bash
# Clone the repository
git clone https://github.com/Josiah-O/thrift-crowd-staking.git
cd thrift-crowd-staking

# Set environment variables
export BLOCKFROST_API_KEY=your_blockfrost_api_key
export CARDANO_NETWORK=preprod

# Start the application
docker-compose up --build
```

### Access the Application

- **Frontend**: http://localhost:3000
- **Backend API**: http://localhost:8080
- **Database**: PostgreSQL on port 5432

## Usage

1. **Install Lace Wallet**: Get the [Lace browser extension](https://www.lace.io/)
2. **Connect Wallet**: Click "Connect Wallet" and approve the connection
3. **Create CSG**: Set up a new Community Savings Group
4. **Join CSG**: Browse and participate in existing groups
5. **Earn Rewards**: Receive real Cardano staking rewards

## Development

### Environment Variables

```bash
# Backend
DATABASE_URL=postgresql://postgres:password@db:5432/thrift_crowd_staking
BLOCKFROST_API_KEY=your_blockfrost_api_key
CARDANO_NETWORK=preprod

# Frontend
NEXT_PUBLIC_API_URL=http://localhost:8080
```

### Local Development

```bash
# Backend
cd backend
stack build
stack run

# Frontend  
cd frontend
npm install
npm run dev
```

## API Endpoints

```
GET  /api/health          - Health check
POST /api/create-csg      - Create new CSG
POST /api/join-csg/{id}   - Join existing CSG
POST /api/claim-reward/{id} - Claim staking rewards
POST /api/close-csg/{id}  - Close CSG
GET  /api/list-csgs       - List active CSGs
POST /api/withdraw/{id}   - Withdraw funds
```

## Documentation

- [Technical Guide](docs/technical_guide.md) - Architecture and implementation details
- [Cardano Configuration](docs/cardano-config.md) - Environment and network setup

## Contributing

We welcome contributions! Please follow these steps:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Code Standards

- **Haskell**: Follow HLint recommendations
- **TypeScript**: Use ESLint + Prettier
- **Documentation**: Update docs for any changes
- **Testing**: Include tests for new features

## Roadmap

- [ ] GitHub project management board
- [ ] Multi-signature wallet support
- [ ] Mobile wallet integration
- [ ] Governance token implementation
- [ ] Advanced analytics dashboard

## License

This project is licensed under the BSD 3-Clause License - see the [LICENSE](LICENSE) file for details.

## Support

- **Issues**: [GitHub Issues](https://github.com/Josiah-O/thrift-crowd-staking/issues)
- **Discussions**: [GitHub Discussions](https://github.com/Josiah-O/thrift-crowd-staking/discussions)
- **Email**: buildwithjosiah@gmail.com
- **Twitter**: [@Real_dev0](https://twitter.com/Real_dev0)

## Acknowledgments

- [Cardano Foundation](https://cardanofoundation.org/)
- [IOG](https://iohk.io/)
- [Emurgo](https://emurgo.io/)
- [Lace Wallet](https://www.lace.io/)
- [Blockfrost](https://blockfrost.io/)

---

**Built with ❤️ on Cardano**
