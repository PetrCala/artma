import React, { ReactElement } from "react"
import Footer from "@/components/Footer/Footer"
import styles from "./index.module.scss"
import CONST from "@/CONST"
import { useTheme } from "next-themes"
import classNames from "classnames"

const Dashboard: React.FC = (): ReactElement => {
  let { theme } = useTheme()
  return (
    <div className={styles.commonPageContainer}>
      <h3>{CONST.APP_NAME_SHORT}</h3>
      <p>{CONST.APP_MOTTO}</p>
      <p className={styles.themeTest}>hello</p>
    </div>
  )
}

export default Dashboard
