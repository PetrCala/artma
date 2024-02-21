/**
 * Constructs a string by joining the values of the given fileTypes object with a comma separator.
 *
 * @param fileTypes - An object containing file types as key-value pairs.
 * @returns A string representing the joined values of the fileTypes object.
 * @example
 * const acceptedFilesString = constructAcceptedFilesString(CONST.ACCEPTED_FILE_TYPES);
 */
function constructAcceptedFilesString(
  fileTypes: Record<string, string>
): string {
  return Object.values(fileTypes).join(", ")
}

export { constructAcceptedFilesString }
