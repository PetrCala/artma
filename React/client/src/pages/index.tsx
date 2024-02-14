import React, { ReactElement } from "react"
import CONST from "@src/CONST"
import Link from "next/link"
import Image from "next/image"
import { MdOutlineFileUpload } from "react-icons/md"
import { VscGraphScatter } from "react-icons/vsc"
import { PiGraphFill } from "react-icons/pi"
import styles from "@styles/pages/index.module.scss"
import ROUTES from "@src/ROUTES"

const DashboardPage: React.FC = (): ReactElement => {
  return (
    <div className="commonPageContainer">
      <h3>{CONST.APP_NAME_SHORT}</h3>
      <p>{CONST.APP_MOTTO}</p>
      <div className={styles.mainItemsContainer}>
        <Link href={ROUTES.DATA_UPLOAD} className={styles.mainItem}>
          <MdOutlineFileUpload className={styles.mainItemIcon} />
          <div className={styles.mainItemText}>Upload</div>
        </Link>
        <Link href={ROUTES.MODELS} className={styles.mainItem}>
          <PiGraphFill className={styles.mainItemIcon} />
          <div className={styles.mainItemText}>Model</div>
        </Link>
        <Link href={ROUTES.EXPLORATION} className={styles.mainItem}>
          <VscGraphScatter className={styles.mainItemIcon} />
          <div className={styles.mainItemText}>Explore</div>
        </Link>
      </div>
    </div>
  )
}

export default DashboardPage
