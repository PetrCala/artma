import React, { ReactElement } from "react"
import Footer from "@/components/Footer/Footer"
import styles from "./index.module.scss"

const Help: React.FC = (): ReactElement => {
  return (
    <div className={styles.commonPageContainer}>
      <h1>Help</h1>
      <p>Welcome to the help page!</p>
    </div>
  )
}

export default Help

