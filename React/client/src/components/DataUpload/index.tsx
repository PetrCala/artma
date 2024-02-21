import React, { useEffect, useRef } from "react"
import Dropzone from "dropzone"
import { cn } from "@src/libs/StylesUtils"
import styles from "./index.module.scss"
import type { DataUploadProps } from "./types"
import { constructAcceptedFilesString } from "@src/libs/DataUtils"
import CONST from "@src/CONST"

const DataUpload = React.forwardRef<HTMLFormElement, DataUploadProps>(
  ({ className, ...props }, ref) => {
    const dropzoneRef = useRef<HTMLFormElement>(null) // Local dropzone ref
    const acceptedFiles = constructAcceptedFilesString(
      CONST.DATA_UPLOAD.ACCEPTED_FILE_TYPES
    )

    useEffect(() => {
      let dataUploadZone: Dropzone | null = null

      // Initialize Dropzone on the ref element
      if (dropzoneRef.current) {
        dataUploadZone = new Dropzone(dropzoneRef.current, {
          url: "/target",
          autoProcessQueue: true,
          uploadMultiple: true,
          parallelUploads: CONST.DATA_UPLOAD.PARALLEL_UPLOADS,
          maxFiles: CONST.DATA_UPLOAD.MAX_FILES,
          maxFilesize: CONST.DATA_UPLOAD.FILE_MAX_SIZE,
          acceptedFiles: acceptedFiles,
          addRemoveLinks: true,
          dictDefaultMessage: CONST.DATA_UPLOAD.DEFAULT_MESSAGE,

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
