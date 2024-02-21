import React, { useEffect, useRef } from "react"
import Dropzone from "dropzone"
import classNames from "classnames"
import Footer from "@components/Footer/Footer"
import { cn } from "@libs/utils"
import styles from "./index.module.scss"
import type { DataUploadProps } from "./types"

const DataUpload = React.forwardRef<HTMLFormElement, DataUploadProps>(
  ({ className, ...props }, ref) => {
    const dropzoneRef = useRef<HTMLFormElement>(null) // Local dropzone ref

    useEffect(() => {
      let dataUploadZone: Dropzone | null = null

      // Initialize Dropzone on the ref element
      if (dropzoneRef.current) {
        dataUploadZone = new Dropzone(dropzoneRef.current, {
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
      }

      // Cleanup
      return () => {
        if (dataUploadZone) {
          dataUploadZone.destroy()
        }
      }
    }, [])

    return (
      <form
        className={cn("dropzone", styles.dropzoneForm, className)}
        ref={dropzoneRef}
        {...props}
      />
    )
  }
)
DataUpload.displayName = "DataUpload"

export default DataUpload
