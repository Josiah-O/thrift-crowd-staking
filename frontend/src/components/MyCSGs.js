import React, { useState, useEffect } from 'react';
import { Link } from 'react-router-dom';
import { useWallet } from '../contexts/WalletContext';
import { listCSGs } from '../api/api';
import { toast } from 'react-toastify';

const MyCSGs = () => {
  const [csgs, setCSGs] = useState([]);
  const [isLoading, setIsLoading] = useState(true);
  const { wallet } = useWallet();

  useEffect(() => {
    const fetchMyCSGs = async () => {
      if (!wallet) {
        setIsLoading(false);
        return;
      }

      try {
        const allCSGs = await listCSGs();
        const myAddress = await wallet.getAddress();
        const myCSGs = allCSGs.filter(csg => csg.participants.includes(myAddress));
        setCSGs(myCSGs);
      } catch (error) {
        console.error('Error fetching CSGs:', error);
        toast.error('Failed to load your CSGs');
      } finally {
        setIsLoading(false);
      }
    };

    fetchMyCSGs();
  }, [wallet]);

  if (isLoading) {
    return <div className="text-center">Loading your CSGs...</div>;
  }

  if (!wallet) {
    return <div className="text-center">Please connect your wallet to view your CSGs</div>;
  }

  return (
    <div>
      <h1 className="text-3xl font-bold mb-6">My Crowd Stake Groups</h1>
      {csgs.length > 0 ? (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {csgs.map((csg) => (
            <div key={csg.csgId} className="border rounded-lg p-4 shadow-md">
              <h3 className="text-xl font-semibold mb-2">{csg.name}</h3>
              <p>Participants: {csg.participants.length} / {csg.maxParticipants}</p>
              <p>Contribution: {csg.contributionAmount / 1000000} ADA</p>
              <p>Total Amount: {csg.totalAmount / 1000000} ADA</p>
              <p>Status: {csg.active ? 'Active' : 'Ended'}</p>
              <Link to={/csg/} className="text-indigo-600 hover:text-indigo-800 mt-2 inline-block">View Details</Link>
            </div>
          ))}
        </div>
      ) : (
        <p>You haven't joined any CSGs yet. <Link to="/" className="text-indigo-600 hover:text-indigo-800">Find a CSG to join</Link> or <Link to="/create-csg" className="text-indigo-600 hover:text-indigo-800">create your own</Link>!</p>
      )}
    </div>
  );
};

export default MyCSGs;
