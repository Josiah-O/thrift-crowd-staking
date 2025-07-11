import React, { createContext, useState, useContext, useEffect } from 'react';
import toast from 'react-hot-toast';

const WalletContext = createContext();

export const useWallet = () => useContext(WalletContext);

export const WalletProvider = ({ children }) => {
  const [wallet, setWallet] = useState(null);
  const [network, setNetwork] = useState(null);

  const checkWalletAvailability = () => {
    return window.cardano && window.cardano.lace ? 'lace' : null;
  };

  useEffect(() => {
    const checkWallet = async () => {
      await new Promise(resolve => setTimeout(resolve, 2000));
      const availableWallet = checkWalletAvailability();
      if (availableWallet) {
        setWallet({ name: availableWallet, api: null });
      }
    };
    checkWallet();
  }, []);

  const connectWallet = async (targetNetwork = 'testnet') => {
    if (!window.cardano || !window.cardano.lace) {
      toast.error('Lace wallet not detected. Please install the Lace extension.');
      return;
    }

    try {
      const api = await window.cardano.lace.enable();
      if (!api) {
        throw new Error('Failed to enable Lace wallet API');
      }
      
      const networkId = await api.getNetworkId();
      const isTestnet = networkId === 0;
      if ((targetNetwork === 'testnet' && !isTestnet) || (targetNetwork === 'mainnet' && isTestnet)) {
        toast.error(`Please switch to Cardano ${targetNetwork} in your Lace wallet.`);
        return;
      }

      const [address] = await api.getUsedAddresses();
      setWallet({ name: 'lace', api, address });
      setNetwork(targetNetwork);
      toast.success(`Lace wallet connected successfully to ${targetNetwork}!`);
    } catch (error) {
      console.error('Wallet connection error:', error);
      toast.error('Failed to connect wallet. Please try again.');
    }
  };

  const disconnectWallet = () => {
    setWallet(null);
    setNetwork(null);
    toast.success('Wallet disconnected.');
  };

  return (
    <WalletContext.Provider value={{ wallet, network, connectWallet, disconnectWallet }}>
      {children}
    </WalletContext.Provider>
  );
};
