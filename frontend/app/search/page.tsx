"use client"
import { useState, useEffect } from "react"
import Link from "next/link"
import { Header } from "@/components/header"
import { Input } from "@/components/ui/input"
import { GroupCard } from "@/components/group-card"
import { Search, ArrowLeft } from "lucide-react"
import type { CSG } from "@/lib/types"
import { listCsgs } from "@/lib/api"

export default function SearchPage() {
  const [searchTerm, setSearchTerm] = useState("")
  const [allGroups, setAllGroups] = useState<CSG[]>([])
  const [isLoading, setIsLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)

  useEffect(() => {
    const fetchGroups = async () => {
      try {
        const groups = await listCsgs()
        setAllGroups(groups)
      } catch (err) {
        setError(err instanceof Error ? err.message : "Failed to fetch groups.")
      } finally {
        setIsLoading(false)
      }
    }
    fetchGroups()
  }, [])

  const filteredGroups = allGroups.filter(
    (group) =>
      group.csgName.toLowerCase().includes(searchTerm.toLowerCase()) ||
      group.csgId.toLowerCase().includes(searchTerm.toLowerCase()),
  )

  const SkeletonCard = () => (
    <div className="bg-neutral-900 border-neutral-700 rounded-lg p-4 space-y-4 animate-pulse">
      <div className="flex justify-between items-start">
        <div className="space-y-2">
          <div className="h-4 bg-neutral-800 rounded w-32"></div>
          <div className="h-3 bg-neutral-800 rounded w-40"></div>
        </div>
        <div className="h-5 bg-neutral-800 rounded w-16"></div>
      </div>
      <div className="grid grid-cols-2 gap-4">
        <div className="space-y-2">
          <div className="h-3 bg-neutral-800 rounded w-16"></div>
          <div className="h-4 bg-neutral-800 rounded w-24"></div>
        </div>
        <div className="space-y-2">
          <div className="h-3 bg-neutral-800 rounded w-20"></div>
          <div className="h-4 bg-neutral-800 rounded w-12"></div>
        </div>
      </div>
    </div>
  )

  return (
    <>
      <Header isConnected={true} />
      <main className="flex-1 p-4 md:p-6 lg:p-8">
        <div className="max-w-5xl mx-auto">
          <Link href="/" className="flex items-center text-sm text-neutral-400 hover:text-orange-500 mb-6">
            <ArrowLeft className="w-4 h-4 mr-2" />
            Back to Dashboard
          </Link>

          <div className="mb-8">
            <h1 className="text-3xl font-bold tracking-wider text-white">Find a Group</h1>
            <p className="text-neutral-400 mt-2">Search for existing Crowd Stake Groups to join.</p>
          </div>

          <div className="relative mb-8">
            <Search className="absolute left-4 top-1/2 -translate-y-1/2 w-5 h-5 text-neutral-400" />
            <Input
              placeholder="Search by group name or ID..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              className="pl-12 h-12 text-lg bg-neutral-800 border-neutral-600 text-white placeholder-neutral-500"
            />
          </div>

          {isLoading ? (
            <div className="grid gap-4 md:grid-cols-2 xl:grid-cols-3">
              {[...Array(3)].map((_, i) => (
                <SkeletonCard key={i} />
              ))}
            </div>
          ) : error ? (
            <div className="text-center py-24 text-red-500">{error}</div>
          ) : filteredGroups.length > 0 ? (
            <div className="grid gap-4 md:grid-cols-2 xl:grid-cols-3">
              {filteredGroups.map((group) => (
                <Link key={group.csgId} href={`/group/${group.csgId}`}>
                  <GroupCard group={group} />
                </Link>
              ))}
            </div>
          ) : (
            <div className="text-center py-24 bg-neutral-900/50 rounded-lg">
              <Search className="mx-auto h-12 w-12 text-neutral-600" />
              <h3 className="mt-4 text-lg font-medium text-white">No Groups Found</h3>
              <p className="mt-1 text-sm text-neutral-400">Try a different search term.</p>
            </div>
          )}
        </div>
      </main>
    </>
  )
}
