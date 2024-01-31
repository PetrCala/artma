import React, { ReactElement } from "react"
import Footer from "@/components/Footer/Footer"
import styles from "./index.module.scss"
import CONST from "@/CONST"

const Dashboard: React.FC = (): ReactElement => {
  return (
    <div className={styles.commonPageContainer}>
      <h3>{CONST.APP_NAME_SHORT}</h3>
      <p>{CONST.APP_MOTTO}</p>
    </div>
  )
}

export default Dashboard
