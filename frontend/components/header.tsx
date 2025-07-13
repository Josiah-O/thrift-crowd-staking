"use client"

import Link from "next/link"
import { Button } from "@/components/ui/button"
import { Wallet, LogOut } from "lucide-react"
import { useWallet } from "@/components/wallet-provider"

interface HeaderProps {
  onConnectClick?: () => void
}

export function Header({ onConnectClick }: HeaderProps) {
  const { isConnected, isAuthenticated, address, user, disconnect } = useWallet()

  const truncateAddress = (addr: string) => {
    if (!addr) return ""
    return `${addr.substring(0, 10)}...${addr.substring(addr.length - 4)}`
  }

  return (
    <header className="sticky top-0 z-50 w-full border-b border-neutral-700 bg-neutral-900/95 backdrop-blur supports-[backdrop-filter]:bg-neutral-900/60">
      <div className="container flex h-16 items-center">
        <div className="mr-4 flex">
          <Link href="/" className="flex items-center space-x-2">
            <Wallet className="h-6 w-6 text-orange-500" />
            <span className="font-bold tracking-wider">THRIFT CROWD STAKING</span>
          </Link>
        </div>
        <div className="flex flex-1 items-center justify-end space-x-2">
          {isConnected && isAuthenticated ? (
            <div className="flex items-center gap-4">
              <span className="text-sm text-neutral-400 font-mono hidden md:block">
                {truncateAddress(address)}
              </span>
              <span className="text-xs text-green-400 hidden lg:block">
                ● Authenticated
              </span>
              <Button onClick={disconnect} variant="ghost" className="text-neutral-400 hover:text-red-500">
                <LogOut className="w-4 h-4 mr-2" />
                Disconnect
              </Button>
            </div>
          ) : (
            <Button onClick={onConnectClick} className="bg-orange-500 hover:bg-orange-600 text-white">
              <Wallet className="w-4 h-4 mr-2" />
              Connect Wallet
            </Button>
          )}
        </div>
      </div>
    </header>
  )
}
