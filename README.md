# Thrift Crowd Staking

Thrift Crowd Staking is a decentralized application (dApp) built on the Cardano blockchain that enables users to create and participate in Crowd Stake Groups (CSGs). This platform aims to provide financial inclusion and peer-to-peer lending opportunities, especially in regions with limited access to traditional banking services.

## Features

- Create and manage Crowd Stake Groups
- Join existing CSGs
- Claim rewards from CSG participation
- Transparent and secure transactions using Cardano blockchain
- User-friendly interface for managing multiple CSGs

## Architecture
![diagram](https://github.com/user-attachments/assets/74f041a3-6da2-4612-9e83-277111e9fc0f)

## Technology Stack

- Smart Contracts: Plutus
- Backend: Haskell (Servant)
- Frontend: React with Redux
- Database: PostgreSQL
- Blockchain: Cardano (Testnet and Mainnet)

## Getting Started

### Prerequisites

- Cardano Node (latest version)
- Haskell and Stack
- Node.js and npm
- PostgreSQL
- Daedalus Wallet or Yoroi Wallet (Testnet and Mainnet versions)

### Installation

1. Clone the repository:
   \\\
   git clone https://github.com/yourusername/thrift-crowd-staking.git
   cd thrift-crowd-staking
   \\\

2. Set up the backend:
   \\\
   cd backend
   stack build
   stack run
   \\\

3. Set up the frontend:
   \\\
   cd frontend
   npm install
   npm start
   \\\

4. Deploy smart contracts (refer to the technical guide for detailed instructions)

### Cardano Testnet Setup

1. Install a Cardano Testnet Wallet:
   - Daedalus Testnet: [Download here](https://daedaluswallet.io/en/testnet/)
   - Yoroi Testnet: Available as a browser extension

2. Create a new wallet in your chosen testnet wallet application.

3. Obtain Testnet ADA:
   - Visit the [Cardano Testnet Faucet](https://developers.cardano.org/en/testnets/cardano/tools/faucet/)
   - Enter your testnet wallet address
   - Request testnet ADA (you can do this once per 24 hours)

4. Configure the application for testnet:
   - In the backend configuration, ensure the Cardano node is pointing to the testnet.
   - Update smart contract deployment scripts to use testnet parameters.

5. Testing transactions:
   - Use your testnet wallet to interact with the dApp.
   - Monitor transactions on the [Cardano Testnet Explorer](https://explorer.cardano-testnet.iohkdev.io/).

Note: Testnet ADA has no real-world value and is only for testing purposes.

### Cardano Mainnet Setup

1. Install a Cardano Mainnet Wallet:
   - Daedalus: [Download here](https://daedaluswallet.io/)
   - Yoroi: Available as a browser extension

2. Create a new wallet in your chosen mainnet wallet application.

3. Obtain Mainnet ADA:
   - Purchase ADA from a cryptocurrency exchange.
   - Transfer the ADA to your mainnet wallet address.

4. Configure the application for mainnet:
   - In the backend configuration, ensure the Cardano node is pointing to the mainnet.
   - Update smart contract deployment scripts to use mainnet parameters.

5. Mainnet transactions:
   - Use your mainnet wallet to interact with the dApp.
   - Monitor transactions on the [Cardano Explorer](https://explorer.cardano.org/en).

Warning: Mainnet transactions involve real ADA with monetary value. Always double-check transactions before confirming.

## Documentation

- [Technical Guide](docs/technical_guide.md)
- [UX Guide](docs/ux_guide.md)
- [Task List](tasks.md)

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments

- Cardano Foundation
- IOHK
- Emurgo

## Contact

For any queries or support, please contact:

- Email: buildwithjosiah@gmail.com
- X (Twitter): @Real_dev0
