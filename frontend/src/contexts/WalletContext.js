import React, { createContext, useState, useContext, useEffect } from 'react';
import { toast } from 'react-toastify';

const WalletContext = createContext();

export const useWallet = () => useContext(WalletContext);

export const WalletProvider = ({ children }) => {
  const [wallet, setWallet] = useState(null);

  useEffect(() => {
    const checkWallet = async () => {
      if (window.cardano) {
        try {
          const walletEnabled = await window.cardano.enable();
          if (walletEnabled) {
            setWallet(window.cardano);
          }
        } catch (error) {
          console.error('Failed to enable wallet:', error);
        }
      }
    };

    checkWallet();
  }, []);

  const connectWallet = async () => {
    if (window.cardano) {
      try {
        const walletEnabled = await window.cardano.enable();
        if (walletEnabled) {
          setWallet(window.cardano);
          toast.success('Wallet connected successfully!');
        }
      } catch (error) {
        console.error('Failed to connect wallet:', error);
        toast.error('Failed to connect wallet. Please try again.');
      }
    } else {
      toast.error('No Cardano wallet detected. Please install a wallet extension.');
    }
  };

  const disconnectWallet = () => {
    setWallet(null);
    toast.info('Wallet disconnected.');
  };

  return (
    <WalletContext.Provider value={{ wallet, connectWallet, disconnectWallet }}>
      {children}
    </WalletContext.Provider>
  );
};
