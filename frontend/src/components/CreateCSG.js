import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { useWallet } from '../contexts/WalletContext';
import { createCSG } from '../api/api';
import { toast } from 'react-toastify';

const CreateCSG = () => {
  const [name, setName] = useState('');
  const [maxParticipants, setMaxParticipants] = useState(10);
  const [contributionAmount, setContributionAmount] = useState(100);
  const [durationWeeks, setDurationWeeks] = useState(52);
  const [isLoading, setIsLoading] = useState(false);
  const { wallet } = useWallet();
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

    setIsLoading(true);
    try {
      const csgData = {
        name,
        maxParticipants,
        contributionAmount,
        durationWeeks,
        creatorId: await wallet.getAddress()
      };
      const newCSG = await createCSG(csgData);
      toast.success('CSG created successfully!');
      navigate(/csg/);
    } catch (error) {
      console.error('Error creating CSG:', error);
      toast.error('Failed to create CSG. Please try again.');
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="max-w-md mx-auto mt-10">
      <h2 className="text-2xl font-bold mb-5">Create a New CSG</h2>
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
            onChange={(e) => setMaxParticipants(parseInt(e.target.value))}
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
            onChange={(e) => setContributionAmount(parseFloat(e.target.value))}
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
            onChange={(e) => setDurationWeeks(parseInt(e.target.value))}
            className="mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-300 focus:ring focus:ring-indigo-200 focus:ring-opacity-50"
            required
            min="1"
            max="52"
          />
        </div>
        <button
          type="submit"
          disabled={isLoading || !wallet}
          className="w-full bg-indigo-600 text-white py-2 px-4 rounded-md hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2 disabled:opacity-50"
        >
          {isLoading ? 'Creating...' : 'Create CSG'}
        </button>
      </form>
    </div>
  );
};

export default CreateCSG;
