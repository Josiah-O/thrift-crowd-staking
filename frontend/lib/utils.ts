import { type ClassValue, clsx } from "clsx"
import { twMerge } from "tailwind-merge"

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs))
}

export const LOVELACE_PER_ADA = 1_000_000

export function toAda(lovelace: number): number {
  return lovelace / LOVELACE_PER_ADA
}

export function toLovelace(ada: number): number {
  return ada * LOVELACE_PER_ADA
}

export function formatAda(lovelace: number): string {
  return toAda(lovelace).toLocaleString(undefined, { minimumFractionDigits: 2, maximumFractionDigits: 6 })
}

export function formatDate(dateString: string): string {
  return new Date(dateString).toLocaleDateString("en-US", {
    year: "numeric",
    month: "short",
    day: "numeric",
    hour: "2-digit",
    minute: "2-digit",
  })
}
