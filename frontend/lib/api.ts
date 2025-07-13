import type { CSG, CreateCSGRequest, JoinCSGRequest } from "./types"

const BASE_URL = process.env.NEXT_PUBLIC_API_URL || "http://localhost:8080"

// Helper to handle API responses
async function handleResponse<T>(response: Response): Promise<T> {
  if (!response.ok) {
    const error = await response.text()
    throw new Error(error || "An API error occurred")
  }
  return response.json()
}

// Pure DApp API endpoints (no authentication required)
export const listCsgs = async (): Promise<CSG[]> => {
  const response = await fetch(`${BASE_URL}/api/list-csgs`, {
    headers: { "Content-Type": "application/json" },
  })
  return handleResponse<CSG[]>(response)
}

export const createCsg = async (data: CreateCSGRequest): Promise<CSG> => {
  const response = await fetch(`${BASE_URL}/api/create-csg`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(data),
  })
  return handleResponse<CSG>(response)
}

export const joinCsg = async (csgId: string, data: JoinCSGRequest): Promise<CSG> => {
  const response = await fetch(`${BASE_URL}/api/join-csg/${csgId}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(data),
  })
  return handleResponse<CSG>(response)
}

export const claimReward = async (csgId: string): Promise<{ message: string }> => {
  const response = await fetch(`${BASE_URL}/api/claim-reward/${csgId}`, { 
    method: "POST",
    headers: { "Content-Type": "application/json" },
  })
  return handleResponse<{ message: string }>(response)
}

export const closeCsg = async (csgId: string): Promise<{ message: string }> => {
  const response = await fetch(`${BASE_URL}/api/close-csg/${csgId}`, { 
    method: "POST",
    headers: { "Content-Type": "application/json" },
  })
  return handleResponse<{ message: string }>(response)
}

export const withdraw = async (csgId: string, amount: number): Promise<{ message: string }> => {
  const response = await fetch(`${BASE_URL}/api/withdraw/${csgId}`, { 
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ withdrawAmount: amount }),
  })
  return handleResponse<{ message: string }>(response)
}
