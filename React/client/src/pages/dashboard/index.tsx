import React, { ReactElement } from "react"
import styles from "./index.module.scss"
import CONST from "@/CONST"

const Dashboard: React.FC = (): ReactElement => {
  return (
    <div className="commonPageContainer">
      <h3>{CONST.APP_NAME_SHORT}</h3>
      <p>{CONST.APP_MOTTO}</p>
      <p className={styles.themeTest}>hello</p>
    </div>
  )
}

export default Dashboard
