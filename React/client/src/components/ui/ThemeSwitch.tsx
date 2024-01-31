// app/components/ThemeSwitch.tsx
"use client"

import { FiSun, FiMoon } from "react-icons/fi"
import { useState, useEffect, useImperativeHandle, forwardRef } from "react"
import { useTheme } from "next-themes"
import Image from "next/image"
import CONST from "@/CONST"

export interface ThemeSwitchRef {
  toggleTheme: () => void
}

interface ThemeSwitchProps {
  className?: string
}

export const ThemeSwitch = forwardRef<ThemeSwitchRef, ThemeSwitchProps>(
  (props, ref) => {
    const { className } = props
    const [mounted, setMounted] = useState(false)
    const { setTheme, resolvedTheme } = useTheme()

    useEffect(() => setMounted(true), []) // Only runs in the client (not during the initial server render)

    useImperativeHandle(ref, () => ({
      toggleTheme() {
        if (resolvedTheme === CONST.THEME.DARK) {
          setTheme(CONST.THEME.LIGHT)
        } else {
          setTheme(CONST.THEME.DARK)
        }
      },
    }))

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
          className={className}
        />
      )

    if (resolvedTheme === CONST.THEME.DARK) {
      return <FiSun className={className} />
    }

    if (resolvedTheme === CONST.THEME.LIGHT) {
      return <FiMoon className={className} />
    }
  }
)
