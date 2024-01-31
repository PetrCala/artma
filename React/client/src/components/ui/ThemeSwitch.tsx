// app/components/ThemeSwitch.tsx
"use client"

import { FiSun, FiMoon } from "react-icons/fi"
import { useState, useEffect } from "react"
import { useTheme } from "next-themes"
import Image from "next/image"
import CONST from "@/CONST"

export default function ThemeSwitch() {
  const [mounted, setMounted] = useState(false)
    const { setTheme, resolvedTheme } = useTheme()

  useEffect(() => setMounted(true), []) // Only runs in the client (not during the initial server render)

  if (!mounted)
    return (
      <Image
        src={CONST.THEME.PLACEHOLDER}
        width={36}
        height={36}
        sizes="36x36"
        alt="Loading Light/Dark Toggle"
        priority={false}
        title="Loading Light/Dark Toggle"
        style={{ cursor: "pointer" }}
      />
    )

    if (resolvedTheme === "dark") {
      return <FiSun onClick={() => setTheme(CONST.THEME.LIGHT)} />
    }

    if (resolvedTheme === "light") {
      return <FiMoon onClick={() => setTheme(CONST.THEME.DARK)} />
    }
}
