import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import toast from 'react-hot-toast';
import { WalletProvider } from '../contexts/WalletContext';
import CreateCSG from './CreateCSG';
import { createCSG } from '../api/api';

jest.mock('../api/api');
jest.mock('react-hot-toast');

const renderWithRouter = (ui, { route = '/' } = {}) => {
  window.history.pushState({}, 'Test page', route);
  return render(ui, { wrapper: BrowserRouter });
};

describe('CreateCSG Component', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  test('renders CreateCSG form', () => {
    renderWithRouter(
      <WalletProvider>
        <CreateCSG />
      </WalletProvider>
    );

    expect(screen.getByText('Create New Crowd Stake Group')).toBeInTheDocument();
    expect(screen.getByLabelText('Group Name')).toBeInTheDocument();
    expect(screen.getByLabelText('Max Participants')).toBeInTheDocument();
    expect(screen.getByLabelText('Contribution Amount (ADA)')).toBeInTheDocument();
    expect(screen.getByLabelText('Duration (weeks)')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Create Group' })).toBeInTheDocument();
  });

  test('submits form with valid data', async () => {
    const mockNavigate = jest.fn();
    jest.mock('react-router-dom', () => ({
      ...jest.requireActual('react-router-dom'),
      useNavigate: () => mockNavigate,
    }));

    createCSG.mockResolvedValue({ csgId: 'test-csg-id' });

    renderWithRouter(
      <WalletProvider>
        <CreateCSG />
      </WalletProvider>
    );

    fireEvent.change(screen.getByLabelText('Group Name'), { target: { value: 'Test CSG' } });
    fireEvent.change(screen.getByLabelText('Max Participants'), { target: { value: '10' } });
    fireEvent.change(screen.getByLabelText('Contribution Amount (ADA)'), { target: { value: '100' } });
    fireEvent.change(screen.getByLabelText('Duration (weeks)'), { target: { value: '52' } });

    fireEvent.click(screen.getByRole('button', { name: 'Create Group' }));

    await waitFor(() => {
      expect(createCSG).toHaveBeenCalledWith({
        name: 'Test CSG',
        maxParticipants: 10,
        contributionAmount: 100000000,
        durationWeeks: 52,
      });
      expect(toast.success).toHaveBeenCalledWith('CSG created successfully!');
      expect(mockNavigate).toHaveBeenCalledWith('/csg/test-csg-id');
    });
  });

  test('displays error message on API failure', async () => {
    createCSG.mockRejectedValue(new Error('API Error'));

    renderWithRouter(
      <WalletProvider>
        <CreateCSG />
      </WalletProvider>
    );

    fireEvent.change(screen.getByLabelText('Group Name'), { target: { value: 'Test CSG' } });
    fireEvent.change(screen.getByLabelText('Max Participants'), { target: { value: '10' } });
    fireEvent.change(screen.getByLabelText('Contribution Amount (ADA)'), { target: { value: '100' } });
    fireEvent.change(screen.getByLabelText('Duration (weeks)'), { target: { value: '52' } });

    fireEvent.click(screen.getByRole('button', { name: 'Create Group' }));

    await waitFor(() => {
      expect(toast.error).toHaveBeenCalledWith('Failed to create CSG. Please try again.');
    });
  });
});
