// CSG types
export interface CSG {
  id: string
  name: string
  participants: string[]
  stakeAmount: number
  duration: number
  startTime: string
  endTime: string
  status: string
}

export interface CreateCSGRequest {
  name: string
  stakeAmount: number
  duration: number
}

export interface JoinCSGRequest {
  stakeAmount: number
}
