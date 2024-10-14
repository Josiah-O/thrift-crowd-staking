import React, { useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';
import toast from 'react-hot-toast';
import { getCSG, claimReward } from '../api/api';
import { useWallet } from '../contexts/WalletContext';

function CSGDetails() {
  const { csgId } = useParams();
  const [csg, setCSG] = useState(null);
  const { wallet } = useWallet();

  useEffect(() => {
    const fetchCSG = async () => {
      try {
        const csgData = await getCSG(csgId);
        setCSG(csgData);
      } catch (error) {
        console.error('Error fetching CSG:', error);
        toast.error('Failed to fetch CSG details');
      }
    };

    fetchCSG();
  }, [csgId]);

  const handleClaimReward = async () => {
    if (!wallet) {
      toast.error('Please connect your wallet to claim rewards');
      return;
    }

    try {
      const result = await claimReward(csgId, { claimantId: wallet.getAddress() });
      if (result.success) {
        toast.success(`Successfully claimed ${result.amount} ADA`);
        // Refresh CSG data
        const updatedCSG = await getCSG(csgId);
        setCSG(updatedCSG);
      } else {
        toast.error(result.message);
      }
    } catch (error) {
      console.error('Error claiming reward:', error);
      toast.error('Failed to claim reward');
    }
  };

  if (!csg) {
    return <div>Loading...</div>;
  }

  return (
    <div>
      <h2 className="text-2xl font-bold mb-4">CSG Details</h2>
      <div className="bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4">
        <p><strong>ID:</strong> {csg.id}</p>
        <p><strong>Name:</strong> {csg.name}</p>
        <p><strong>Description:</strong> {csg.description}</p>
        <p><strong>Total Stake:</strong> {csg.totalStake} ADA</p>
        <p><strong>Number of Participants:</strong> {csg.participants.length}</p>
        <button
          onClick={handleClaimReward}
          className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline mt-4"
        >
          Claim Reward
        </button>
      </div>
    </div>
  );
}

export default CSGDetails;
