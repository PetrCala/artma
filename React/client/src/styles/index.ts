// import { defaultTheme } from "./theme"
import sizing from "./utils/sizing"
import spacing from "./utils/spacing"
import { StylesObject } from "./utils/types"

// interface Styles {
//   [key: string]: React.CSSProperties
// }

// (theme: string) =>
const commonStyles: StylesObject = {
  // Import utils
  ...sizing,
  ...spacing,

  extraSmallImage: {
    width: 24,
    height: 24,
  },
  smallImage: {
    width: 36,
    height: 36,
  },
  mediumImage: {
    width: 48,
    height: 48,
  },
  largeImage: {
    width: 64,
    height: 64,
  },
  extraLargeImage: {
    width: 96,
    height: 96,
  },
}

export default commonStyles
