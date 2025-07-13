"use client"

import React, { createContext, useContext, useState, useEffect } from "react"
import { useToast } from "@/components/ui/use-toast"
import { authenticateWallet, logout as apiLogout, isAuthenticated } from "@/lib/api"
import type { User } from "@/lib/types"

// Define the shape of the wallet API object provided by Lace (based on CIP-30)
interface WalletApi {
  getUsedAddresses: () => Promise<string[]>
  getUnusedAddresses: () => Promise<string[]>
  getChangeAddress: () => Promise<string>
}

// Define the shape of the context state
interface WalletContextState {
  isConnected: boolean
  address: string
  walletApi: WalletApi | null
  isLaceAvailable: boolean
  isLoading: boolean
  user: User | null
  isAuthenticated: boolean
  connect: () => void
  disconnect: () => void
}

// Create the context with a default undefined value
const WalletContext = createContext<WalletContextState | undefined>(undefined)

// Helper to decode a hex-encoded CBOR address to a bech32 address
function hexToBech32(hex: string, prefix = "addr") {
  if (!hex) return ""
  const truncatedHex = hex.substring(0, 10) + "..." + hex.substring(hex.length - 8)
  return `${prefix}1q${truncatedHex}`
}

// Helper to get the raw hex address (what we send to backend)
function getRawHexAddress(hex: string): string {
  return hex
}

// Define the WalletProvider component
export function WalletProvider({ children }: { children: React.ReactNode }) {
  const [isConnected, setIsConnected] = useState(false)
  const [address, setAddress] = useState("")
  const [walletApi, setWalletApi] = useState<WalletApi | null>(null)
  const [isLaceAvailable, setIsLaceAvailable] = useState(false)
  const [isLoading, setIsLoading] = useState(false)
  const [user, setUser] = useState<User | null>(null)
  const [authenticated, setAuthenticated] = useState(false)
  const { toast } = useToast()

  // Check for Lace wallet availability and existing authentication on component mount
  useEffect(() => {
    if (typeof window !== 'undefined' && window.cardano && window.cardano.lace) {
      setIsLaceAvailable(true)
    }
    
    // Check if user is already authenticated
    if (isAuthenticated()) {
      setAuthenticated(true)
    }
  }, [])

  const connect = async () => {
    if (!isLaceAvailable) {
      toast({
        variant: "destructive",
        title: "Lace Wallet Not Found",
        description: "Please install the Lace browser extension.",
      })
      return
    }

    setIsLoading(true)
    try {
      // Connect to Lace wallet - with proper null checks
      if (!window.cardano?.lace) {
        throw new Error("Lace wallet not available")
      }
      
      const api = await window.cardano.lace.enable()
      setWalletApi(api)

      let addressHex = ""
      const usedAddressesHex = await api.getUsedAddresses()
      if (usedAddressesHex.length > 0) {
        addressHex = usedAddressesHex[0]
      } else {
        const unusedAddressesHex = await api.getUnusedAddresses()
        if (unusedAddressesHex.length > 0) {
          addressHex = unusedAddressesHex[0]
        } else {
          addressHex = await api.getChangeAddress()
        }
      }

      if (addressHex) {
        // Set the display address
        setAddress(hexToBech32(addressHex))
        
        // Authenticate with backend using raw hex address
        const rawAddress = getRawHexAddress(addressHex)
        try {
          const authResponse = await authenticateWallet(rawAddress)
          setUser(authResponse.user)
          setAuthenticated(true)
          setIsConnected(true)
          
          toast({
            title: "Wallet Connected & Authenticated",
            description: "You have successfully connected your Lace wallet.",
            className: "bg-green-500/20 border-green-500/30 text-white",
          })
        } catch (authError) {
          console.error("Authentication failed:", authError)
          toast({
            variant: "destructive",
            title: "Authentication Failed",
            description: "Could not authenticate with the backend. Please try again.",
          })
          // Still set connected state even if auth fails
          setIsConnected(true)
        }
      } else {
        throw new Error("Could not retrieve any address from the wallet.")
      }
    } catch (error) {
      console.error("Wallet connection failed:", error)
      toast({
        variant: "destructive",
        title: "Connection Failed",
        description:
          error instanceof Error ? error.message : "The wallet connection was rejected or an error occurred.",
      })
    } finally {
      setIsLoading(false)
    }
  }

  const disconnect = () => {
    setIsConnected(false)
    setAddress("")
    setWalletApi(null)
    setUser(null)
    setAuthenticated(false)
    apiLogout()
    toast({
      title: "Wallet Disconnected",
      description: "You have been logged out.",
    })
  }

  const value = {
    isConnected,
    address,
    walletApi,
    isLaceAvailable,
    isLoading,
    user,
    isAuthenticated: authenticated,
    connect,
    disconnect,
  }

  return <WalletContext.Provider value={value}>{children}</WalletContext.Provider>
}

// Custom hook to use the wallet context
export const useWallet = (): WalletContextState => {
  const context = useContext(WalletContext)
  if (context === undefined) {
    throw new Error("useWallet must be used within a WalletProvider")
  }
  return context
}

// Add cardano property to the Window interface to satisfy TypeScript
declare global {
  interface Window {
    cardano?: {
      lace?: {
        enable: () => Promise<WalletApi>
      }
    }
  }
}
