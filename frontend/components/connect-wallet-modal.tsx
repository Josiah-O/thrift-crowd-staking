"use client"

import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle } from "@/components/ui/dialog"
import { Button } from "@/components/ui/button"
import Image from "next/image"
import { useWallet } from "./wallet-provider"
import { Loader2 } from "lucide-react"

interface ConnectWalletModalProps {
  isOpen: boolean
  onClose: () => void
  onConnect: () => void
  isLaceAvailable: boolean
}

export function ConnectWalletModal({ isOpen, onClose, onConnect, isLaceAvailable }: ConnectWalletModalProps) {
  const { isLoading } = useWallet()

  return (
    <Dialog open={isOpen} onOpenChange={onClose}>
      <DialogContent className="bg-neutral-900 border-neutral-700 text-white">
        <DialogHeader>
          <DialogTitle className="text-xl font-bold tracking-wider">Connect a Wallet</DialogTitle>
          <DialogDescription className="text-neutral-400">
            {isLaceAvailable
              ? "Please connect your Lace wallet to continue."
              : "Lace wallet extension not found. Please install it to proceed."}
          </DialogDescription>
        </DialogHeader>
        <div className="flex justify-center py-4">
          {isLaceAvailable ? (
            <Button
              variant="outline"
              className="h-24 w-48 flex flex-col gap-2 border-neutral-700 hover:border-orange-500 hover:bg-neutral-800 bg-transparent"
              onClick={onConnect}
              disabled={isLoading}
            >
              {isLoading ? (
                <Loader2 className="h-8 w-8 animate-spin" />
              ) : (
                <>
                  <Image src="/lace-logo.png" alt="Lace Wallet Logo" width={40} height={40} />
                  <span className="font-bold">Lace Wallet</span>
                </>
              )}
            </Button>
          ) : (
            <a href="https://www.lace.io/" target="_blank" rel="noopener noreferrer">
              <Button
                variant="outline"
                className="h-24 w-48 flex flex-col gap-2 border-neutral-700 hover:border-orange-500 hover:bg-neutral-800 bg-transparent"
              >
                <Image src="/lace-logo.png" alt="Lace Wallet Logo" width={40} height={40} />
                <span className="font-bold">Install Lace</span>
              </Button>
            </a>
          )}
        </div>
      </DialogContent>
    </Dialog>
  )
}
