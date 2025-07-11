import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { useWallet } from '../contexts/WalletContext';
import { createCSG } from '../api/api';
import toast from 'react-hot-toast';

const CreateCSG = () => {
  const [name, setName] = useState('');
  const [maxParticipants, setMaxParticipants] = useState(10);
  const [contributionAmount, setContributionAmount] = useState(100);
  const [durationWeeks, setDurationWeeks] = useState(52);
  const [isLoading, setIsLoading] = useState(false);
  const { wallet, network, connectWallet } = useWallet();
  const navigate = useNavigate();

  const validateForm = () => {
    if (!name.trim()) {
      toast.error('Please enter a name for the CSG');
      return false;
    }
    if (maxParticipants < 2 || maxParticipants > 100) {
      toast.error('Number of participants must be between 2 and 100');
      return false;
    }
    if (contributionAmount <= 0) {
      toast.error('Contribution amount must be greater than 0');
      return false;
    }
    if (durationWeeks < 1 || durationWeeks > 52) {
      toast.error('Duration must be between 1 and 52 weeks');
      return false;
    }
    return true;
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    if (!validateForm()) return;

    if (!wallet || !wallet.api) {
      toast.error('Please connect your Lace wallet first');
      return;
    }

    setIsLoading(true);
    try {
      const csgData = {
        name,
        maxParticipants,
        contributionAmount,
        durationWeeks,
        creatorId: wallet.address,
        network
      };
      console.log('Sending CSG data:', csgData); // Log the data being sent
      const newCSG = await createCSG(csgData);
      console.log('Received new CSG:', newCSG); // Log the response
      toast.success('CSG created successfully!');
      navigate('/csg/');
    } catch (error) {
      console.error('Error creating CSG:', error);
      if (error.response) {
        // The request was made and the server responded with a status code
        // that falls out of the range of 2xx
        console.error('Server responded with:', error.response.data);
        console.error('Status code:', error.response.status);
        toast.error(`Failed to create CSG: ${error.response.data.message || 'Server error'}`);
      } else if (error.request) {
        // The request was made but no response was received
        console.error('No response received:', error.request);
        toast.error('Failed to create CSG: No response from server');
      } else {
        // Something happened in setting up the request that triggered an Error
        console.error('Error setting up request:', error.message);
        toast.error(`Failed to create CSG: ${error.message}`);
      }
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="max-w-md mx-auto mt-10">
      <h2 className="text-2xl font-bold mb-5">Create a New CSG</h2>
      {!wallet || !wallet.api ? (
        <button
          onClick={() => connectWallet('testnet')}
          className="w-full bg-indigo-600 text-white py-2 px-4 rounded-md hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2"
        >
          Connect Lace Wallet
        </button>
      ) : (
        <form onSubmit={handleSubmit} className="space-y-4">
          <div>
            <label htmlFor="name" className="block text-sm font-medium text-gray-700">Name</label>
            <input
              type="text"
              id="name"
              value={name}
              onChange={(e) => setName(e.target.value)}
              className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-300 focus:ring focus:ring-indigo-200 focus:ring-opacity-50"
              required
            />
          </div>
          <div>
            <label htmlFor="maxParticipants" className="block text-sm font-medium text-gray-700">Max Participants</label>
            <input
              type="number"
              id="maxParticipants"
              value={maxParticipants}
              onChange={(e) => setMaxParticipants(parseInt(e.target.value) || 0)}
              className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-300 focus:ring focus:ring-indigo-200 focus:ring-opacity-50"
              required
              min="2"
              max="100"
            />
          </div>
          <div>
            <label htmlFor="contributionAmount" className="block text-sm font-medium text-gray-700">Contribution Amount (ADA)</label>
            <input
              type="number"
              id="contributionAmount"
              value={contributionAmount}
              onChange={(e) => setContributionAmount(parseFloat(e.target.value) || 0)}
              className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-300 focus:ring focus:ring-indigo-200 focus:ring-opacity-50"
              required
              min="0.1"
              step="0.1"
            />
          </div>
          <div>
            <label htmlFor="durationWeeks" className="block text-sm font-medium text-gray-700">Duration (weeks)</label>
            <input
              type="number"
              id="durationWeeks"
              value={durationWeeks}
              onChange={(e) => setDurationWeeks(parseInt(e.target.value) || 0)}
              className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-300 focus:ring focus:ring-indigo-200 focus:ring-opacity-50"
              required
              min="1"
              max="52"
            />
          </div>
          <button
            type="submit"
            disabled={isLoading}
            className="w-full bg-indigo-600 text-white py-2 px-4 rounded-md hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2 disabled:opacity-50"
          >
            {isLoading ? 'Creating...' : 'Create CSG'}
          </button>
        </form>
      )}
    </div>
  );
};

export default CreateCSG;
