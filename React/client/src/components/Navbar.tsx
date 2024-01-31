import Link from "next/link"
import {
  FaYoutube,
  FaTwitter,
  FaGithub,
  FaLaptop,
  FaQuestion,
  FaBook,
} from "react-icons/fa"
import styles from "@/styles/components/Navbar.module.css"
import commonStyles from "@/styles/commonStyles.module.css"
import { ThemeSwitch, ThemeSwitchRef } from "./ui/ThemeSwitch"
import CONST from "@/CONST"
import Image from "next/image"
import { useRef } from "react"

export default function Navbar() {
  const themeSwitchRef = useRef<ThemeSwitchRef>(null)

  return (
    <nav className={styles.navigation}>
      <div className={styles.container}>
        <h1 className={styles.leftContainer}>
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
        </h1>
        <div className={styles.rightContainer}>
          <Link
            href="https://www.youtube.com/@DaveGrayTeachesCode"
            className={styles.rightContainerIconWrapper}
          >
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
