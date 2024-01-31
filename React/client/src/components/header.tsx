import React from "react"
import { MainNav } from "./main-nav"
// import UserButton from "./user-button"
import styles from "@/styles/components/Header.module.css"
import useTheme from "@/hooks/useTheme";

export default function Header() {
  const { theme } = useTheme();
  return (
    <header className={styles.header}>
      <div className={styles.container}>
        {theme} theme
        {/* <MainNav /> */}
        {/* <UserButton /> Implement this later */}
      </div>
    </header>
  )
}