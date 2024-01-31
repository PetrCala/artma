import React from "react"
import { MainNav } from "./main-nav"
// import UserButton from "./user-button"
import styles from "@/styles/components/Header.module.css"

export default function Header() {
  return (
    <header className={styles.header}>
      <div className={styles.container}>
        Hello World
        {/* <MainNav /> */}
        {/* <UserButton /> Implement this later */}
      </div>
    </header>
  )
}