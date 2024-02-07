import React, { ReactElement } from "react"
import styles from "./index.module.scss"
import CONST from "@/CONST"
import Link from "next/link"
import Image from "next/image"
import { MdOutlineFileUpload } from "react-icons/md"
import { VscGraphScatter } from "react-icons/vsc"
import { PiGraphFill } from "react-icons/pi"

const Dashboard: React.FC = (): ReactElement => {
  return (
    <div className="commonPageContainer">
      <h3>{CONST.APP_NAME_SHORT}</h3>
      <p>{CONST.APP_MOTTO}</p>
      <div className={styles.mainItemsContainer}>
        <Link href="/dashboard" className={styles.mainItem}>
          <MdOutlineFileUpload className={styles.mainItemIcon} />
          <div className={styles.mainItemText}>Upload</div>
        </Link>
        <Link href="/dashboard" className={styles.mainItem}>
          <PiGraphFill className={styles.mainItemIcon} />
          <div className={styles.mainItemText}>Model</div>
        </Link>
        <Link href="/dashboard" className={styles.mainItem}>
          <VscGraphScatter className={styles.mainItemIcon} />
          <div className={styles.mainItemText}>Explore</div>
        </Link>
      </div>
    </div>
  )
}

export default Dashboard
