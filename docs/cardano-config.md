# Cardano Configuration Guide

## ⚠️ Security Notice
**NEVER commit wallet keys or API keys to git!** This guide shows how to set up environment variables securely.

## Required Environment Variables

### 1. Create Local .env File
Create a `.env` file in your project root (already in .gitignore):

```bash
# Copy and paste this, replace with your actual values:
BLOCKFROST_PROJECT_ID=your_blockfrost_project_id_here
CARDANO_WALLET_ADDRESS=your_wallet_address_here
```

### 2. Set Up Blockfrost API
1. Go to https://blockfrost.io
2. Create account and get a **preprod** project ID
3. Add to your `.env` file:
   ```
   BLOCKFROST_PROJECT_ID=preprodABC123XYZ789
   ```

### 3. Create Cardano Wallet in Codespace
```bash
# Create keys directory (ignored by git)
mkdir -p keys

# Generate wallet (run in Codespace terminal)
cardano-cli address key-gen \
  --verification-key-file keys/wallet.vkey \
  --signing-key-file keys/wallet.skey

# Generate address
cardano-cli address build \
  --payment-verification-key-file keys/wallet.vkey \
  --out-file keys/wallet.addr \
  --testnet-magic 1

# Add address to .env file
echo "CARDANO_WALLET_ADDRESS=$(cat keys/wallet.addr)" >> .env
```

### 4. Fund Your Wallet
- Use [Cardano Testnet Faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/)
- Send test ADA to your wallet address: `cat keys/wallet.addr`

### 5. Load Environment Variables
```bash
# Load .env file before running docker-compose
source .env
docker-compose up -d
```

## How Docker Compose Works
Your `docker-compose.yml` uses environment variable references:
```yaml
environment:
  - BLOCKFROST_PROJECT_ID=${BLOCKFROST_PROJECT_ID}
  - CARDANO_WALLET_ADDRESS=${CARDANO_WALLET_ADDRESS}
```

This pulls values from your local `.env` file without exposing them in git.

## Production Deployment
For production, set environment variables in your hosting platform:
- **GitHub Actions**: Use repository secrets
- **Heroku**: Use config vars
- **AWS**: Use Parameter Store or Secrets Manager
- **Docker**: Use docker secrets

## File Structure
```
your-project/
├── .env                 # YOUR SECRETS (never commit!)
├── .env.example        # Template (safe to commit)
├── keys/               # Wallet files (never commit!)
│   ├── wallet.skey     # Signing key
│   ├── wallet.vkey     # Verification key
│   └── wallet.addr     # Address
├── docker-compose.yml  # Uses ${VARIABLE} references
└── .gitignore          # Includes .env and keys/
```

## Transaction Types

The application creates real Cardano transactions for:
- **Join CSG**: Sends ADA from user wallet to CSG contract address
- **Claim Rewards**: Sends rewards from CSG contract to user address  
- **Close CSG**: Returns all funds from CSG contract to creator
- **Withdraw**: Sends specified amount from CSG contract to user address 