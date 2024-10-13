import React, { useState, useEffect } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import { useWallet } from '../contexts/WalletContext';
import { getCSG, joinCSG } from '../api/api';
import toast from 'react-hot-toast';

const JoinCSG = () => {
  const params = useParams();
  const csgId = params.csgId;
  const [csg, setCSG] = useState(null);
  const [isLoading, setIsLoading] = useState(true);
  const { wallet } = useWallet();
  const navigate = useNavigate();

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

  const handleJoin = async () => {
    if (!wallet) {
      toast.error('Please connect your wallet to join the CSG');
      return;
    }

    try {
      const result = await joinCSG(csgId, { participantId: wallet.getAddress(), amount: csg.contributionAmount });
      if (result) {
        toast.success('Successfully joined the CSG');
        navigate(/csg/);
      } else {
        toast.error('Failed to join the CSG');
      }
    } catch (error) {
      console.error('Error joining CSG:', error);
      toast.error('Failed to join the CSG');
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
      <h1 className="text-3xl font-bold mb-6">Join {csg.name}</h1>
      <div className="bg-white shadow overflow-hidden sm:rounded-lg mb-6">
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
          </dl>
        </div>
      </div>
      <p className="mb-4">By joining this CSG, you agree to contribute {csg.contributionAmount / 1000000} ADA.</p>
      <button
        onClick={handleJoin}
        disabled={!wallet || csg.participants.length >= csg.maxParticipants}
        className="bg-indigo-600 text-white px-4 py-2 rounded-md hover:bg-indigo-700 disabled:opacity-50 disabled:cursor-not-allowed"
      >
        {!wallet ? 'Connect Wallet to Join' : 'Join CSG'}
      </button>
    </div>
  );
};

export default JoinCSG;
