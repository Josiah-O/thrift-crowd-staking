"use client"

import { useState } from "react"
import Link from "next/link"
import { Header } from "@/components/header"
import { Button } from "@/components/ui/button"
import { Card, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { ConnectWalletModal } from "@/components/connect-wallet-modal"
import { Wallet, PlusCircle, Search } from "lucide-react"
import { useWallet } from "@/components/wallet-provider"

export default function DashboardPage() {
  const [isModalOpen, setIsModalOpen] = useState(false)
  const { isConnected, isAuthenticated, user, address, connect, isLaceAvailable } = useWallet()

  return (
    <>
      <Header onConnectClick={() => setIsModalOpen(true)} />
      <main className="flex-1 p-4 md:p-6 lg:p-8">
        {!isConnected || !isAuthenticated ? (
          <div className="flex flex-col items-center justify-center h-[70vh] text-center">
            <Wallet className="w-16 h-16 text-orange-500 mb-4" />
            <h1 className="text-2xl font-bold tracking-wider mb-2">Connect Your Wallet</h1>
            <p className="text-neutral-400 max-w-md mb-6">
              To create or join a Community Savings Group, please connect your Cardano wallet.
            </p>
            <Button onClick={() => setIsModalOpen(true)} className="bg-orange-500 hover:bg-orange-600 text-white">
              Connect Wallet
            </Button>
          </div>
        ) : (
          <div className="flex flex-col items-center justify-center h-[70vh]">
            <div className="text-center mb-12">
              <h1 className="text-3xl font-bold tracking-wider text-white">
                Welcome, {user?.walletAddress ? address : "User"}
              </h1>
              <p className="text-neutral-400">What would you like to do?</p>
            </div>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-8 w-full max-w-4xl">
              <Link href="/create">
                <Card className="bg-neutral-900 border-neutral-700 h-full hover:border-orange-500/80 transition-all duration-300 transform hover:-translate-y-1 cursor-pointer">
                  <CardHeader className="flex flex-col items-center text-center p-8">
                    <PlusCircle className="w-12 h-12 text-orange-500 mb-4" />
                    <CardTitle className="text-xl font-bold tracking-wider">Create a CSG</CardTitle>
                    <CardDescription className="text-neutral-400 mt-2">
                      Create a new Community Savings Group and define its staking goals.
                    </CardDescription>
                  </CardHeader>
                </Card>
              </Link>
              <Link href="/search">
                <Card className="bg-neutral-900 border-neutral-700 h-full hover:border-orange-500/80 transition-all duration-300 transform hover:-translate-y-1 cursor-pointer">
                  <CardHeader className="flex flex-col items-center text-center p-8">
                    <Search className="w-12 h-12 text-orange-500 mb-4" />
                    <CardTitle className="text-xl font-bold tracking-wider">Browse CSGs</CardTitle>
                    <CardDescription className="text-neutral-400 mt-2">
                      Search and join existing Community Savings Groups.
                    </CardDescription>
                  </CardHeader>
                </Card>
              </Link>
            </div>
          </div>
        )}
      </main>
      <ConnectWalletModal 
        isOpen={isModalOpen} 
        onClose={() => setIsModalOpen(false)}
        onConnect={connect}
        isLaceAvailable={isLaceAvailable}
      />
    </>
  )
}
