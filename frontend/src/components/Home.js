import React, { useState, useEffect } from 'react';
import { Link } from 'react-router-dom';
import { listCSGs } from '../api/api';
import { useWallet } from '../contexts/WalletContext';

const Home = () => {
  const [csgs, setCSGs] = useState([]);
  const [isLoading, setIsLoading] = useState(true);
  const { wallet, connectWallet, disconnectWallet } = useWallet();

  useEffect(() => {
    const fetchCSGs = async () => {
      try {
        const fetchedCSGs = await listCSGs();
        setCSGs(fetchedCSGs);
      } catch (error) {
        console.error('Error fetching CSGs:', error);
      } finally {
        setIsLoading(false);
      }
    };

    fetchCSGs();
  }, []);

  if (isLoading) {
    return <div className="text-center">Loading CSGs...</div>;
  }

  return (
    <div>
      <h1 className="text-3xl font-bold mb-6">Welcome to Thrift Crowd Staking</h1>
      <p className="mb-4">Join or create a Crowd Stake Group (CSG) to start saving and earning together!</p>
      
      {wallet ? (
        <div>
          <p>Connected: {wallet.address}</p>
          <button onClick={disconnectWallet} className="bg-red-600 text-white px-4 py-2 rounded-md hover:bg-red-700 mb-4 inline-block">Disconnect Wallet</button>
        </div>
      ) : (
        <button onClick={() => connectWallet('testnet')} className="bg-green-600 text-white px-4 py-2 rounded-md hover:bg-green-700 mb-4 inline-block">Connect Wallet</button>
      )}

      <Link to="/create-csg" className="bg-indigo-600 text-white px-4 py-2 rounded-md hover:bg-indigo-700 mb-8 inline-block">Create New CSG</Link>
      
      <h2 className="text-2xl font-bold mt-8 mb-4">Active CSGs</h2>
      {csgs.length > 0 ? (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {csgs.map((csg) => (
            <div key={csg.csgId} className="border rounded-lg p-4 shadow-md">
              <h3 className="text-xl font-semibold mb-2">{csg.name}</h3>
              <p>Participants: {csg.participants.length} / {csg.maxParticipants}</p>
              <p>Contribution: {csg.contributionAmount / 1000000} ADA</p>
              <Link to={`/csg/${csg.csgId}`} className="text-indigo-600 hover:text-indigo-800 mt-2 inline-block">View Details</Link>
            </div>
          ))}
        </div>
      ) : (
        <p>No active CSGs found. Be the first to create one!</p>
      )}
    </div>
  );
};

export default Home;
