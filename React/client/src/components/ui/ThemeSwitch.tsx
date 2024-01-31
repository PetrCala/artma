// app/components/ThemeSwitch.tsx
"use client"

import { FiSun, FiMoon } from "react-icons/fi"
import { useState, useEffect, useImperativeHandle } from "react"
import { useTheme } from "next-themes"
import Image from "next/image"
import CONST from "@/CONST"

export default function ThemeSwitch() {
  const [mounted, setMounted] = useState(false)
  const { setTheme, resolvedTheme } = useTheme()

  useEffect(() => setMounted(true), []) // Only runs in the client (not during the initial server render)

  // useImperativeHandle(ref, () => ({
  //   toggleTheme() {
  //     setTheme(prevTheme => prevTheme === CONST.THEME.DARK ? CONST.THEME.LIGHT : CONST.THEME.DARK);
  //   }
  // }));


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
      />
    )

  if (resolvedTheme === CONST.THEME.DARK) {
    return <FiSun onClick={() => setTheme(CONST.THEME.LIGHT)} />
  }

  if (resolvedTheme === CONST.THEME.LIGHT) {
    return <FiMoon onClick={() => setTheme(CONST.THEME.DARK)} />
  }
}


  // if (resolvedTheme === CONST.THEME.DARK) {
  //   return (
  //     <button
  //       onClick={() => setTheme(CONST.THEME.LIGHT)}
  //       className={commonStyles.clickableIconWrapper}
  //     >
  //       <FiSun className={commonStyles.clickableIcon} />
  //     </button>
  //   )
  // }

  // if (resolvedTheme === CONST.THEME.LIGHT) {
  //   return (
  //     <button
  //       onClick={() => setTheme(CONST.THEME.DARK)}
  //       className={commonStyles.clickableIconWrapper}
  //     >
  //       <FiMoon className={commonStyles.clickableIcon} />
  //     </button>
  //   )
  // }