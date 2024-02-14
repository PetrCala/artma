"use client"

import Link from "next/link"
import { FaBook } from "react-icons/fa"
import { ThemeSwitch, ThemeSwitchRef } from "@components/ui/ThemeSwitch"
import CONST from "@src/CONST"
import Image from "next/image"
import { useRef } from "react"
import classNames from "classnames"
import ROUTES from "@src/ROUTES"
import styles from "@styles/components/Navbar/Navbar.module.scss"

export default function Navbar() {
  const themeSwitchRef = useRef<ThemeSwitchRef>(null)

  return (
    <nav className={styles.navigation}>
      <div className={styles.container}>
        <h6 className={styles.leftContainer}>
          <Link
            href={ROUTES.HOME}
            className={classNames(
              "horizontallyCenter linkContainer",
              styles.leftContainerLinkContainer
            )}
          >
            <Image
              src={CONST.IMAGES.ARTMA_LOGO}
              width={36}
              height={36}
              sizes="36x36"
              alt="Application Logo"
              priority={false}
              title="Application Logo"
            />
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
