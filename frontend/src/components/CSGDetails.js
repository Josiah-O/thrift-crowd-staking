import React, { useState, useEffect } from 'react';
import { useParams, Link } from 'react-router-dom';
import { useWallet } from '../contexts/WalletContext';
import { getCSG, claimReward, endCSG } from '../api/api';
import { toast } from 'react-toastify';

const CSGDetails = () => {
  const { csgId } = useParams();
  const [csg, setCSG] = useState(null);
  const [isLoading, setIsLoading] = useState(true);
  const { wallet } = useWallet();

  useEffect(() => {
    const fetchCSG = async () => {
      try {
        const fetchedCSG = await getCSG(csgId);
        setCSG(fetchedCSG);
      } catch (error) {
        console.error('Error fetching CSG:', error);
        toast.error('Failed to load CSG details');
      } finally {
        setIsLoading(false);
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
        toast.success(Successfully claimed  ADA);
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

  const handleEndCSG = async () => {
    if (!wallet) {
      toast.error('Please connect your wallet to end the CSG');
      return;
    }

    try {
      const result = await endCSG(csgId);
      if (result.success) {
        toast.success('CSG ended successfully');
        // Refresh CSG data
        const updatedCSG = await getCSG(csgId);
        setCSG(updatedCSG);
      } else {
        toast.error(result.message);
      }
    } catch (error) {
      console.error('Error ending CSG:', error);
      toast.error('Failed to end CSG');
    }
  };

  if (isLoading) {
    return <div className="text-center">Loading CSG details...</div>;
  }

  if (!csg) {
    return <div className="text-center">CSG not found</div>;
  }

  return (
    <div>
      <h1 className="text-3xl font-bold mb-6">{csg.name}</h1>
      <div className="bg-white shadow overflow-hidden sm:rounded-lg">
        <div className="px-4 py-5 sm:px-6">
          <h3 className="text-lg leading-6 font-medium text-gray-900">CSG Details</h3>
        </div>
        <div className="border-t border-gray-200">
          <dl>
            <div className="bg-gray-50 px-4 py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6">
              <dt className="text-sm font-medium text-gray-500">Participants</dt>
              <dd className="mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2">{csg.participants.length} / {csg.maxParticipants}</dd>
            </div>
            <div className="bg-white px-4 py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6">
              <dt className="text-sm font-medium text-gray-500">Contribution Amount</dt>
              <dd className="mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2">{csg.contributionAmount / 1000000} ADA</dd>
            </div>
            <div className="bg-gray-50 px-4 py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6">
              <dt className="text-sm font-medium text-gray-500">Total Amount</dt>
              <dd className="mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2">{csg.totalAmount / 1000000} ADA</dd>
            </div>
            <div className="bg-white px-4 py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6">
              <dt className="text-sm font-medium text-gray-500">Reward Pool</dt>
              <dd className="mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2">{csg.rewardPool / 1000000} ADA</dd>
            </div>
            <div className="bg-gray-50 px-4 py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6">
              <dt className="text-sm font-medium text-gray-500">Status</dt>
              <dd className="mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2">{csg.active ? 'Active' : 'Ended'}</dd>
            </div>
          </dl>
        </div>
      </div>
      <div className="mt-6 flex space-x-4">
        {csg.active && (
          <>
            <Link to={/join-csg/} className="bg-indigo-600 text-white px-4 py-2 rounded-md hover:bg-indigo-700">Join CSG</Link>
            <button onClick={handleClaimReward} className="bg-green-600 text-white px-4 py-2 rounded-md hover:bg-green-700">Claim Reward</button>
            <button onClick={handleEndCSG} className="bg-red-600 text-white px-4 py-2 rounded-md hover:bg-red-700">End CSG</button>
          </>
        )}
      </div>
    </div>
  );
};

export default CSGDetails;
