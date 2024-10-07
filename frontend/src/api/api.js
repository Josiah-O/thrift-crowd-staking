import axios from 'axios';
import { toast } from 'react-toastify';

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
    const response = await api.post('/csg/create', csgData);
    return response.data;
  } catch (error) {
    console.error('Error creating CSG:', error);
    throw error;
  }
};

export const joinCSG = async (csgId, joinData) => {
  try {
    const response = await api.post('/csg/join/' + csgId, joinData);
    return response.data;
  } catch (error) {
    console.error('Error joining CSG:', error);
    throw error;
  }
};

export const claimReward = async (csgId, claimData) => {
  try {
    const response = await api.post('/csg/claim/' + csgId, claimData);
    return response.data;
  } catch (error) {
    console.error('Error claiming reward:', error);
    throw error;
  }
};

export const endCSG = async (csgId) => {
  try {
    const response = await api.post('/csg/end/' + csgId);
    return response.data;
  } catch (error) {
    console.error('Error ending CSG:', error);
    throw error;
  }
};

export const listCSGs = async (page = 1, pageSize = 10) => {
  try {
    const response = await api.get('/csg/list?page=' + page + '&pageSize=' + pageSize);
    return response.data;
  } catch (error) {
    console.error('Error listing CSGs:', error);
    throw error;
  }
};

export const getCSG = async (csgId) => {
  try {
    const response = await api.get('/csg/' + csgId);
    return response.data;
  } catch (error) {
    console.error('Error getting CSG:', error);
    throw error;
  }
};

export const withdraw = async (csgId, withdrawData) => {
  try {
    const response = await api.post('/csg/withdraw/' + csgId, withdrawData);
    return response.data;
  } catch (error) {
    console.error('Error withdrawing from CSG:', error);
    throw error;
  }
};
