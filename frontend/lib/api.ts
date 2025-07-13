import type { CSG, CreateCSGRequest, JoinCSGRequest, AuthResponse } from "./types"

const BASE_URL = process.env.NEXT_PUBLIC_API_URL || "http://localhost:8080"

// Token management
export const getToken = (): string | null => {
  if (typeof window !== 'undefined') {
    return localStorage.getItem('jwt_token')
  }
  return null
}

export const setToken = (token: string): void => {
  if (typeof window !== 'undefined') {
    localStorage.setItem('jwt_token', token)
  }
}

export const removeToken = (): void => {
  if (typeof window !== 'undefined') {
    localStorage.removeItem('jwt_token')
  }
}

// Get authenticated headers
function getAuthHeaders(): Record<string, string> {
  const token = getToken()
  const headers: Record<string, string> = {
    "Content-Type": "application/json",
  }
  
  if (token) {
    headers["Authorization"] = `Bearer ${token}`
  }
  
  return headers
}

// Helper to handle API responses
async function handleResponse<T>(response: Response): Promise<T> {
  if (!response.ok) {
    if (response.status === 401) {
      // Token expired or invalid
      removeToken()
      throw new Error("Authentication required. Please reconnect your wallet.")
    }
    const error = await response.text()
    throw new Error(error || "An API error occurred")
  }
  return response.json()
}

// Wallet-based authentication - PURE WALLET ONLY
export const authenticateWallet = async (walletAddress: string): Promise<AuthResponse> => {
  const response = await fetch(`${BASE_URL}/api/auth/wallet`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ 
      walletAddress: walletAddress
    }),
  })
  
  const result = await handleResponse<AuthResponse>(response)
  if (result.token) {
    setToken(result.token)
  }
  return result
}

// Remove old email-based functions
// const registerWallet = async (walletAddress: string): Promise<AuthResponse> => {

export const logout = (): void => {
  removeToken()
}

// Check if user is authenticated
export const isAuthenticated = (): boolean => {
  return getToken() !== null
}

// Protected API endpoints (require authentication)
export const listCsgs = async (): Promise<CSG[]> => {
  const response = await fetch(`${BASE_URL}/api/list-csgs`, {
    headers: getAuthHeaders(),
  })
  return handleResponse<CSG[]>(response)
}

export const createCsg = async (data: CreateCSGRequest): Promise<CSG> => {
  const response = await fetch(`${BASE_URL}/api/create-csg`, {
    method: "POST",
    headers: getAuthHeaders(),
    body: JSON.stringify(data),
  })
  return handleResponse<CSG>(response)
}

export const joinCsg = async (csgId: string, data: JoinCSGRequest): Promise<CSG> => {
  const response = await fetch(`${BASE_URL}/api/join-csg/${csgId}`, {
    method: "POST",
    headers: getAuthHeaders(),
    body: JSON.stringify(data),
  })
  return handleResponse<CSG>(response)
}

export const claimReward = async (csgId: string): Promise<{ message: string }> => {
  const response = await fetch(`${BASE_URL}/api/claim-reward/${csgId}`, { 
    method: "POST",
    headers: getAuthHeaders(),
  })
  return handleResponse<{ message: string }>(response)
}

export const closeCsg = async (csgId: string): Promise<{ message: string }> => {
  const response = await fetch(`${BASE_URL}/api/close-csg/${csgId}`, { 
    method: "POST",
    headers: getAuthHeaders(),
  })
  return handleResponse<{ message: string }>(response)
}

export const withdraw = async (csgId: string, amount: number): Promise<{ message: string }> => {
  const response = await fetch(`${BASE_URL}/api/withdraw/${csgId}`, { 
    method: "POST",
    headers: getAuthHeaders(),
    body: JSON.stringify({ withdrawAmount: amount }),
  })
  return handleResponse<{ message: string }>(response)
}
