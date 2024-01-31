"use client"

import { Session } from "next-auth"
import { SessionProvider } from "next-auth/react"
import { ThemeProvider } from "next-themes"

type ProvidersProps = {
  children: React.ReactNode
  session: Session
}

export function Providers({ children, session }: ProvidersProps) {
  return (
    <ThemeProvider attribute="class" defaultTheme="light" enableSystem> {/* def...system*/}
      <SessionProvider session={session}>
        {children}
      </SessionProvider>
    </ThemeProvider>
  )
}
