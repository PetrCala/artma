import React from "react"
import { MainNav } from "./main-nav"
// import UserButton from "./user-button"
import styles from "@/styles/components/Header.module.css"
import commonStyles from "@/styles/commonStyles.module.css"
import useTheme from "@/hooks/useTheme"
import CONST from "@/CONST"

export default function Header() {
  const { theme } = useTheme()

  const backgroundClass =
    theme === CONST.THEME.LIGHT
      ? commonStyles.backgroundLight
      : commonStyles.backgroundDark

  return (
    <header className={`${styles.header} ${backgroundClass}`}>
      <div className={styles.container}>
        {theme}
        {/* <MainNav /> */}
        {/* <UserButton /> Implement this later */}
      </div>
    </header>
  )
}
