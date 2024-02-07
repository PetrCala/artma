"use client"

import Link from "next/link"
import { FaBook } from "react-icons/fa"
import { ThemeSwitch, ThemeSwitchRef } from "../ui/ThemeSwitch"
import CONST from "@/CONST"
import Image from "next/image"
import { useRef } from "react"
import ROUTES from "@/ROUTES"
import styles from "./Navbar.module.scss"

export default function Navbar() {
  const themeSwitchRef = useRef<ThemeSwitchRef>(null)

  return (
    <nav className={styles.navigation}>
      <div className={styles.container}>
        <h6 className={styles.leftContainer}>
          <Image
            src={CONST.IMAGES.ARTMA_LOGO}
            width={36}
            height={36}
            sizes="36x36"
            alt="Application Logo"
            priority={false}
            title="Application Logo"
          />
          <Link
            href="/dashboard"
            className="text-white/90 no-underline hover:text-white"
          >
            {CONST.APP_NAME_SHORT}
          </Link>
        </h6>
        <div className={styles.rightContainer}>
          <Link href={ROUTES.HELP} className={styles.rightContainerIconWrapper}>
            <FaBook className={styles.rightContainerIcon} />
          </Link>

          <button
            onClick={() => themeSwitchRef.current?.toggleTheme()}
            className={styles.rightContainerIconWrapper}
          >
            <ThemeSwitch
              className={styles.rightContainerIcon}
              ref={themeSwitchRef}
            />
          </button>
        </div>
      </div>
    </nav>
  )
}
