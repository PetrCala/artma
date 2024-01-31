import React, { ReactElement } from "react"
import Footer from "@/components/footer"

const Dashboard: React.FC = (): ReactElement => {
  return (
    <div>
      <h1>Dashboard</h1>
      <p>Welcome to the dashboard page!</p>
      <Footer />
    </div>
  )
}

export default Dashboard
