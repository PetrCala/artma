import Link from "next/link"
import { FaYoutube, FaTwitter, FaGithub, FaLaptop } from "react-icons/fa"
import styles from "@styles/components/Footer/Footer.module.scss"
import CONST from "@src/CONST"

export default function Footer() {
  return (
    <footer className={styles.footer}>
      <div className={styles.container}>
        <div className={styles.leftContainer}>
          &copy; {new Date().getFullYear()} {CONST.APP_NAME_SHORT}
        </div>
        <div className={styles.rightContainer}>
          <Link
            className="text-white/90 hover:text-white"
            href="https://www.youtube.com/@DaveGrayTeachesCode"
          >
            <FaYoutube />
          </Link>
          <Link
            className="text-white/90 hover:text-white"
            href="https://courses.davegray.codes/"
          >
            <FaLaptop />
          </Link>
          <Link
            className="text-white/90 hover:text-white"
            href="https://github.com/gitdagray"
          >
            <FaGithub />
          </Link>
          <Link
            className="text-white/90 hover:text-white"
            href="https://twitter.com/yesdavidgray"
          >
            <FaTwitter />
          </Link>
        </div>
      </div>
    </footer>
  )
}
