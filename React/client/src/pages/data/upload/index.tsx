import React, { ReactElement } from "react"
import Footer from "@components/Footer/Footer"
import styles from "@styles/pages/data/upload/index.module.scss"

const DataUploadPage: React.FC = (): ReactElement => {
  return (
    <div className={styles.commonPageContainer}>
      <h1>Help</h1>
      <p>Welcome to the data upload page!</p>
    </div>
  )
}

export default DataUploadPage
