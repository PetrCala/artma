import React from "react"
import { MainNav } from "./main-nav"
// import UserButton from "./user-button"
import styles from "@/styles/components/Header.module.css"
import commonStyles from "@/styles/commonStyles.module.css"
import CONST from "@/CONST"
import ThemeSwitch from "./ThemeSwitch"

export default function Header() {

  return (
    <header className={styles.header}>
      <div className={styles.container}>
        <ThemeSwitch/>
        {/* <MainNav /> */}
        {/* <UserButton /> Implement this later */}
      </div>
    </header>
  )
}
