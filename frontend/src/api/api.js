import axios from 'axios';
import toast from 'react-hot-toast';  // Updated import

const API_BASE_URL = process.env.REACT_APP_API_URL || 'http://localhost:8080';

const api = axios.create({
  baseURL: API_BASE_URL,
});

api.interceptors.response.use(
  (response) => response,
  (error) => {
    const message = error.response?.data?.message || 'An unexpected error occurred';
    toast.error(message);
    return Promise.reject(error);
  }
);

export const createCSG = async (csgData) => {
  try {
    // Make sure this endpoint matches your backend
    const response = await axios.post(`${API_BASE_URL}/api/csg`, csgData);
    return response.data;
  } catch (error) {
    console.error('API error:', error);
    throw error;
  }
};

export const joinCSG = async (csgId, joinData) => {
  try {
    const response = await api.post(`/csg/join/${csgId}`, joinData);
    return response.data;
  } catch (error) {
    console.error('Error joining CSG:', error);
    throw error;
  }
};

export const claimReward = async (csgId, claimantData) => {
  try {
    const response = await api.post(`/csg/${csgId}/claim`, claimantData);
    return response.data;
  } catch (error) {
    console.error('Error claiming reward:', error);
    throw error;
  }
};

export const endCSG = async (csgId) => {
  try {
    const response = await api.post(`/csg/end/${csgId}`);
    return response.data;
  } catch (error) {
    console.error('Error ending CSG:', error);
    throw error;
  }
};

export const listCSGs = async (page = 1, pageSize = 10) => {
  try {
    // Make sure this endpoint matches your backend
    const response = await axios.get(`${API_BASE_URL}/api/csg`, {
      params: { page, pageSize }
    });
    return response.data;
  } catch (error) {
    console.error('Error listing CSGs:', error);
    throw error;
  }
};

export const getCSG = async (csgId) => {
  try {
    const response = await api.get(`/csg/${csgId}`);
    return response.data;
  } catch (error) {
    console.error('Error fetching CSG:', error);
    throw error;
  }
};

export const withdraw = async (csgId, withdrawData) => {
  try {
    const response = await api.post(`/csg/withdraw/${csgId}`, withdrawData);
    return response.data;
  } catch (error) {
    console.error('Error withdrawing from CSG:', error);
    throw error;
  }
};
