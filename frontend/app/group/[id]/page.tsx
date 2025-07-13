"use client"

import type React from "react"

import { useState, useEffect } from "react"
import { useParams } from "next/navigation"
import Link from "next/link"
import { Header } from "@/components/header"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Button } from "@/components/ui/button"
import { Input } from "@/components/ui/input"
import { useToast } from "@/components/ui/use-toast"
import { ArrowLeft, Users, Wallet, Gift, X, LogOut, Loader2 } from "lucide-react"
import type { CSG } from "@/lib/types"
import { listCsgs, joinCsg, claimReward, closeCsg, withdraw } from "@/lib/api"
import { toLovelace, formatAda, formatDate } from "@/lib/utils"

export default function GroupDetailPage() {
  const params = useParams()
  const { toast } = useToast()
  const csgId = params.id as string

  const [group, setGroup] = useState<CSG | null>(null)
  const [isLoading, setIsLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)
  const [stakeAmount, setStakeAmount] = useState("")
  const [isSubmitting, setIsSubmitting] = useState(false)

  const fetchGroup = async () => {
    try {
      // In a real app, you'd have a GET /api/csg/:id endpoint.
      // For now, we filter the full list.
      const groups = await listCsgs()
      const foundGroup = groups.find((g) => g.csgId === csgId)
      if (foundGroup) {
        setGroup(foundGroup)
      } else {
        setError("Group not found.")
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to fetch group details.")
    } finally {
      setIsLoading(false)
    }
  }

  useEffect(() => {
    if (csgId) {
      fetchGroup()
    }
  }, [csgId])

  const handleAction = async (action: () => Promise<any>, successMessage: string) => {
    setIsSubmitting(true)
    try {
      await action()
      toast({
        title: "Success",
        description: successMessage,
        className: "bg-green-500/20 border-green-500/30 text-white",
      })
      fetchGroup() // Re-fetch data to show updated state
    } catch (err) {
      toast({
        variant: "destructive",
        title: "Action Failed",
        description: err instanceof Error ? err.message : "An unknown error occurred.",
      })
    } finally {
      setIsSubmitting(false)
    }
  }

  const handleJoin = () =>
    handleAction(
      () => joinCsg(csgId, { joinCsgStakeAmount: toLovelace(Number(stakeAmount)) }),
      `Successfully staked ${stakeAmount} ADA.`,
    )

  const handleClaim = () => handleAction(() => claimReward(csgId), "Rewards claimed successfully.")
  const handleClose = () => handleAction(() => closeCsg(csgId), "Group closed successfully.")
  const handleWithdraw = () => handleAction(() => withdraw(csgId), "Withdrawal successful.")

  if (isLoading) {
    return (
      <div className="flex h-screen items-center justify-center">
        <Loader2 className="w-8 h-8 text-orange-500 animate-spin" />
      </div>
    )
  }

  if (error || !group) {
    return (
      <div className="flex h-screen items-center justify-center text-red-500">
        <p>{error || "Could not load group."}</p>
      </div>
    )
  }

  return (
    <>
      <Header isConnected={true} />
      <main className="flex-1 p-4 md:p-6 lg:p-8">
        <div className="max-w-5xl mx-auto">
          <Link href="/search" className="flex items-center text-sm text-neutral-400 hover:text-orange-500 mb-6">
            <ArrowLeft className="w-4 h-4 mr-2" />
            Back to Search
          </Link>

          <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
            <div className="lg:col-span-2 space-y-6">
              <Card className="bg-neutral-900 border-neutral-700">
                <CardHeader>
                  <CardTitle className="text-2xl font-bold tracking-wider text-white">{group.csgName}</CardTitle>
                  <CardDescription className="text-neutral-500 font-mono pt-1">{group.csgId}</CardDescription>
                </CardHeader>
                <CardContent className="space-y-6">
                  <div>
                    <h4 className="text-sm font-medium text-neutral-400 tracking-wider mb-3">TIMELINE</h4>
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-x-6 gap-y-4 text-sm">
                      <InfoItem label="Status" value={group.csgStatus} />
                      <InfoItem label="Duration" value={`${group.csgDuration} days`} />
                      <InfoItem label="Start Time" value={formatDate(group.csgStartTime)} />
                      <InfoItem label="End Time" value={formatDate(group.csgEndTime)} />
                    </div>
                  </div>
                  <div className="border-t border-neutral-800"></div>
                  <div>
                    <h4 className="text-sm font-medium text-neutral-400 tracking-wider mb-3">STAKE DETAILS</h4>
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-x-6 gap-y-4 text-sm">
                      <InfoItem label="Total Stake" value={`${formatAda(group.csgTotalStake)} ADA`} icon={Wallet} />
                      <InfoItem label="Participants" value={group.csgParticipants.length.toString()} icon={Users} />
                    </div>
                  </div>
                </CardContent>
              </Card>
            </div>

            <aside className="lg:col-span-1">
              <Card className="bg-neutral-900 border-neutral-700 sticky top-24">
                <CardHeader>
                  <CardTitle className="text-lg font-bold tracking-wider text-white">Actions</CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  {group.csgStatus === "Funding" && (
                    <div className="space-y-2">
                      <label htmlFor="stake-amount" className="text-sm font-medium text-neutral-300">
                        Stake Amount (ADA)
                      </label>
                      <Input
                        id="stake-amount"
                        type="number"
                        placeholder="e.g., 500"
                        value={stakeAmount}
                        onChange={(e) => setStakeAmount(e.target.value)}
                        className="bg-neutral-800 border-neutral-600 text-white"
                      />
                      <Button
                        onClick={handleJoin}
                        disabled={!stakeAmount || isSubmitting}
                        className="w-full bg-orange-500 flex items-center justify-center"
                      >
                        {isSubmitting ? (
                          <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                        ) : (
                          <Wallet className="w-4 h-4 mr-2" />
                        )}
                        {isSubmitting ? "Submitting..." : "Join Group"}
                      </Button>
                    </div>
                  )}
                  {group.csgStatus === "Completed" && (
                    <Button
                      onClick={handleClaim}
                      disabled={isSubmitting}
                      className="w-full flex items-center justify-center"
                    >
                      {isSubmitting ? (
                        <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                      ) : (
                        <Gift className="w-4 h-4 mr-2" />
                      )}
                      {isSubmitting ? "Submitting..." : "Claim Reward"}
                    </Button>
                  )}
                  {group.csgStatus !== "Closed" && (
                    <Button
                      onClick={handleClose}
                      disabled={isSubmitting}
                      variant="outline"
                      className="w-full bg-transparent flex items-center justify-center"
                    >
                      {isSubmitting ? (
                        <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                      ) : (
                        <X className="w-4 h-4 mr-2" />
                      )}
                      {isSubmitting ? "Submitting..." : "Close Group"}
                    </Button>
                  )}
                  {group.csgStatus === "Closed" && (
                    <Button
                      onClick={handleWithdraw}
                      disabled={isSubmitting}
                      className="w-full flex items-center justify-center"
                    >
                      {isSubmitting ? (
                        <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                      ) : (
                        <LogOut className="w-4 h-4 mr-2" />
                      )}
                      {isSubmitting ? "Submitting..." : "Withdraw Stake"}
                    </Button>
                  )}
                </CardContent>
              </Card>
            </aside>
          </div>
        </div>
      </main>
    </>
  )
}

const InfoItem = ({ label, value, icon: Icon }: { label: string; value: string; icon?: React.ElementType }) => (
  <div className="flex items-center gap-3">
    {Icon && <Icon className="w-6 h-6 text-neutral-500" />}
    <div>
      <p className="text-neutral-400">{label}</p>
      <p className="font-mono text-white">{value}</p>
    </div>
  </div>
)
