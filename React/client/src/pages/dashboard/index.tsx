import React, { ReactElement } from "react"
import Footer from "@/components/footer"
import commonStyles from "@/styles/commonStyles.module.css"

const Dashboard: React.FC = (): ReactElement => {
  return (
    <div className={commonStyles.pageContainer}>
      <h1>Dashboard</h1>
      <p>Welcome to the dashboard page!</p>
    </div>
  )
}

export default Dashboard
