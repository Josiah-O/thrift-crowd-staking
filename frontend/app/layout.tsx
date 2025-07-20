import type React from "react"
import type { Metadata } from "next"
import { Inter, Roboto_Mono } from "next/font/google"
import "./globals.css"
import { cn } from "@/lib/utils"
import { Toaster } from "@/components/ui/toaster"
import { WalletProvider } from "@/components/wallet-provider"

const fontSans = Inter({
  subsets: ["latin"],
  variable: "--font-sans",
})

const fontMono = Roboto_Mono({
  subsets: ["latin"],
  variable: "--font-mono",
})

export const metadata: Metadata = {
  title: "Thrift Crowd Staking - Cardano dApp",
  description: "Decentralized peer-to-peer lending and staking on the Cardano blockchain.",
    generator: 'v0.dev'
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en" suppressHydrationWarning>
      <body
        className={cn(
          "min-h-screen bg-neutral-900 font-sans text-white antialiased",
          fontSans.variable,
          fontMono.variable,
        )}
      >
        <WalletProvider>
          <div className="relative flex min-h-screen flex-col">{children}</div>
          <Toaster />
        </WalletProvider>
      </body>
    </html>
  )
}
