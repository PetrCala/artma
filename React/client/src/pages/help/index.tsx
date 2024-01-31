import React, { ReactElement } from "react"
import Footer from "@/components/footer"
import commonStyles from "@/styles/commonStyles.module.css"

const Help: React.FC = (): ReactElement => {
  return (
    <div className={commonStyles.pageContainer}>
      <h1>Help</h1>
      <p>Welcome to the help page!</p>
    </div>
  )
}

export default Help

