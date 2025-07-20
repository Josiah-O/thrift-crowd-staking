import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Badge } from "@/components/ui/badge"
import { Users, Wallet } from "lucide-react"
import type { CSG } from "@/lib/types"
import { formatAda, toLovelace } from "@/lib/utils"

interface GroupCardProps {
  group: CSG
}

export function GroupCard({ group }: GroupCardProps) {
  const getStatusColor = (status: string) => {
    switch (status) {
      case "Active":
        return "bg-green-500/20 text-green-400 border-green-500/30"
      case "Completed":
        return "bg-blue-500/20 text-blue-400 border-blue-500/30"
      case "Funding":
        return "bg-orange-500/20 text-orange-400 border-orange-500/30"
      default:
        return "bg-neutral-500/20 text-neutral-400 border-neutral-500/30"
    }
  }

  return (
    <Card className="bg-neutral-900 border-neutral-700 flex flex-col h-full hover:border-orange-500/80 transition-all duration-300 transform hover:-translate-y-1 cursor-pointer">
      <CardHeader>
        <div className="flex justify-between items-start">
          <CardTitle className="text-base font-bold tracking-wider text-white">{group.csgName}</CardTitle>
          <Badge variant="outline" className={getStatusColor(group.csgStatus)}>
            {group.csgStatus.toUpperCase()}
          </Badge>
        </div>
        <CardDescription className="text-neutral-500 font-mono text-xs truncate">{group.csgId}</CardDescription>
      </CardHeader>
      <CardContent className="flex-grow space-y-4">
        <div className="space-y-2">
          <div className="flex justify-between text-xs font-mono">
            <span className="text-neutral-400">Progress</span>
            <span className="text-white">{((group.csgTotalStake / toLovelace(50000)) * 100).toFixed(0)}%</span>
          </div>
          <div className="w-full bg-neutral-800 rounded-full h-1.5">
            <div
              className="bg-orange-500 h-1.5 rounded-full"
              style={{ width: `${(group.csgTotalStake / toLovelace(50000)) * 100}%` }}
            ></div>
          </div>
        </div>
        <div className="grid grid-cols-2 gap-4 text-sm">
          <div className="flex items-center gap-2">
            <Wallet className="w-4 h-4 text-neutral-500" />
            <div>
              <p className="text-xs text-neutral-400">Total Stake</p>
              <p className="font-mono text-white">{formatAda(group.csgTotalStake)} ADA</p>
            </div>
          </div>
          <div className="flex items-center gap-2">
            <Users className="w-4 h-4 text-neutral-500" />
            <div>
              <p className="text-xs text-neutral-400">Participants</p>
              <p className="font-mono text-white">{group.csgParticipants.length}</p>
            </div>
          </div>
        </div>
      </CardContent>
    </Card>
  )
}
