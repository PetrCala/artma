import React, { ReactElement, useRef, useEffect } from "react"
import Dropzone from "dropzone"
import classNames from "classnames"
import Footer from "@components/Footer/Footer"
import styles from "@styles/pages/data/upload/index.module.scss"

const DataUploadPage: React.FC = (): ReactElement => {
  const dropzoneRef = useRef<HTMLFormElement>(null)

  useEffect(() => {
    // Initialize Dropzone on the ref element
    if (dropzoneRef.current) {
      const dataUploadZone = new Dropzone(dropzoneRef.current, {
        url: "/target",
        autoProcessQueue: false,
        uploadMultiple: true,
        parallelUploads: 100,
        maxFiles: 100,
        maxFilesize: 100, // Size in MB
        acceptedFiles: "image/*",
        addRemoveLinks: true,
        // dictDefaultMessage: "Drop files here to upload",
        dictDefaultMessage: "Drop files here to upload",
        // "<span class='dropzoneMessage'>Drop files here to upload</span>",

        accept: function (file: any, done: any) {
          if (file.name == "justinbieber.jpg") {
            done("Naha, you don't.")
          } else {
            done()
          }
        },
      })

      dataUploadZone.on("addedfile", function (file: any) {
        console.log("File added")
      })
      dataUploadZone.on("removedfile", function (file: any) {
        console.log("File removed")
      })
      dataUploadZone.on("error", function (file: any, errorMessage: any) {
        console.log("File error")
      })
      dataUploadZone.on("success", function (file: any) {
        console.log("File success")
      })
      dataUploadZone.on("complete", function (file: any) {
        console.log("File complete")
      })

      // Cleanup
      return () => dataUploadZone.destroy()
    }
  }, [])

  return (
    <div className="commonPageContainer">
      <p>Welcome to the data upload page!</p>
      <form
        ref={dropzoneRef}
        className={classNames("dropzone", styles.dropzoneForm)}
      />
    </div>
  )
}

export default DataUploadPage
