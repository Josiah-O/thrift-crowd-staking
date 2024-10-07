import React from 'react';
import { Link } from 'react-router-dom';
import { useWallet } from '../contexts/WalletContext';

const Header = () => {
  const { wallet, connectWallet, disconnectWallet } = useWallet();

  return (
    <header className="bg-indigo-600">
      <nav className="container mx-auto px-4 py-3 flex items-center justify-between">
        <Link to="/" className="text-white text-xl font-bold">Thrift Crowd Staking</Link>
        <div className="flex items-center">
          <Link to="/" className="text-white mr-4 hover:text-indigo-200">Home</Link>
          <Link to="/create-csg" className="text-white mr-4 hover:text-indigo-200">Create CSG</Link>
          <Link to="/my-csgs" className="text-white mr-4 hover:text-indigo-200">My CSGs</Link>
          {wallet ? (
            <button
              onClick={disconnectWallet}
              className="bg-white text-indigo-600 px-4 py-2 rounded-md hover:bg-indigo-100"
            >
              Disconnect Wallet
            </button>
          ) : (
            <button
              onClick={connectWallet}
              className="bg-white text-indigo-600 px-4 py-2 rounded-md hover:bg-indigo-100"
            >
              Connect Wallet
            </button>
          )}
        </div>
      </nav>
    </header>
  );
};

export default Header;
