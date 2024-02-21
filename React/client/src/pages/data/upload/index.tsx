import React, { ReactElement, useRef, useEffect } from "react"
import Footer from "@components/Footer/Footer"
import DataUpload from "@src/components/DataUpload"

const DataUploadPage: React.FC = (): ReactElement => {
  return (
    <div className="commonPageContainer">
      <p>Welcome to the data upload page!</p>
      <DataUpload />
    </div>
  )
}

export default DataUploadPage
