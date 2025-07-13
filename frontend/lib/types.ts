// Simple authentication types for wallet-based auth - WALLET ONLY
export interface User {
  id: string
  walletAddress: string
  createdAt: string
}

export interface AuthResponse {
  token: string
  user: User
  message: string
}

// CSG types based on backend specification
export interface CSG {
  csgId: string
  csgName: string
  csgOwnerAddress: string
  csgParticipants: string[]
  csgTotalStake: number // in lovelace
  csgDuration: number // in days
  csgStartTime: string
  csgEndTime: string
  csgStatus: "Funding" | "Active" | "Completed" | "Closed"
}

// Request types for CSG operations
export interface CreateCSGRequest {
  createCsgName: string
  createCsgDuration: number // days
  createCsgStakeAmount: number // in lovelace
}

export interface JoinCSGRequest {
  joinCsgStakeAmount: number // in lovelace
}
