import React, { ReactElement } from "react"
import Footer from "@/components/Footer/Footer"
import styles from "./index.module.scss"

const Dashboard: React.FC = (): ReactElement => {
  return (
    <div className={styles.commonPageContainer}>
      <h3>Dashboard</h3>
      <p>Welcome to the dashboard page!</p>
    </div>
  )
}

export default Dashboard
