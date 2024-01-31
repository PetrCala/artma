import React, { ReactElement } from "react"
import Header from "@/components/header"

const Dashboard: React.FC = (): ReactElement => {
  return (
    <div>
      <Header />
      <h1>Dashboard</h1>
      <p>Welcome to the dashboard page!</p>
    </div>
  )
}

export default Dashboard
