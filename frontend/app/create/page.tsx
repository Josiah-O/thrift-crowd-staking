"use client"

import type React from "react"
import { useState } from "react"
import { useRouter } from "next/navigation"
import { Header } from "@/components/header"
import { Button } from "@/components/ui/button"
import { Input } from "@/components/ui/input"
import { Label } from "@/components/ui/label"
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card"
import { ArrowLeft, Loader2 } from "lucide-react"
import Link from "next/link"
import { useToast } from "@/components/ui/use-toast"
import { createCsg } from "@/lib/api"
import { toLovelace } from "@/lib/utils"

export default function CreateGroupPage() {
  const [name, setName] = useState("")
  const [duration, setDuration] = useState("")
  const [stake, setStake] = useState("")
  const [isLoading, setIsLoading] = useState(false)
  const router = useRouter()
  const { toast } = useToast()

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    setIsLoading(true)

    try {
      const newCsg = await createCsg({
        createCsgName: name,
        createCsgDuration: Number(duration),
        createCsgStakeAmount: toLovelace(Number(stake)),
      })

      toast({
        title: "Transaction Successful!",
        description: `Group "${newCsg.csgName}" has been created.`,
        className: "bg-green-500/20 border-green-500/30 text-white",
      })
      router.push(`/group/${newCsg.csgId}`)
    } catch (error) {
      toast({
        variant: "destructive",
        title: "Creation Failed",
        description: error instanceof Error ? error.message : "An unknown error occurred.",
      })
    } finally {
      setIsLoading(false)
    }
  }

  return (
    <>
      <Header isConnected={true} />
      <main className="flex-1 p-4 md:p-6 lg:p-8 flex items-center justify-center">
        <Card className="w-full max-w-2xl bg-neutral-900 border-neutral-700">
          <CardHeader>
            <Link href="/" className="flex items-center text-sm text-neutral-400 hover:text-orange-500 mb-4">
              <ArrowLeft className="w-4 h-4 mr-2" />
              Back to Dashboard
            </Link>
            <CardTitle className="text-2xl font-bold tracking-wider text-white">Create a Crowd Stake Group</CardTitle>
            <CardDescription className="text-neutral-400">
              Define the parameters for your new staking group. This will be deployed on Cardano.
            </CardDescription>
          </CardHeader>
          <form onSubmit={handleSubmit}>
            <CardContent className="space-y-6">
              <div className="space-y-2">
                <Label htmlFor="group-name" className="text-neutral-300">
                  Group Name
                </Label>
                <Input
                  id="group-name"
                  value={name}
                  onChange={(e) => setName(e.target.value)}
                  required
                  className="bg-neutral-800 border-neutral-600 text-white placeholder-neutral-500"
                />
              </div>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <div className="space-y-2">
                  <Label htmlFor="duration" className="text-neutral-300">
                    Duration (days)
                  </Label>
                  <Input
                    id="duration"
                    type="number"
                    value={duration}
                    onChange={(e) => setDuration(e.target.value)}
                    required
                    className="bg-neutral-800 border-neutral-600 text-white placeholder-neutral-500"
                  />
                </div>
                <div className="space-y-2">
                  <Label htmlFor="stake" className="text-neutral-300">
                    Your Initial Stake (ADA)
                  </Label>
                  <Input
                    id="stake"
                    type="number"
                    value={stake}
                    onChange={(e) => setStake(e.target.value)}
                    required
                    className="bg-neutral-800 border-neutral-600 text-white placeholder-neutral-500"
                  />
                </div>
              </div>
            </CardContent>
            <CardFooter>
              <Button
                type="submit"
                disabled={isLoading}
                className="w-full bg-orange-500 hover:bg-orange-600 text-white flex items-center justify-center"
              >
                {isLoading && <Loader2 className="mr-2 h-4 w-4 animate-spin" />}
                {isLoading ? "Submitting to Blockchain..." : "Create Group & Stake"}
              </Button>
            </CardFooter>
          </form>
        </Card>
      </main>
    </>
  )
}
