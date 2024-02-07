// Creating a default array and object this way because objects ({}) and arrays ([]) are not stable types.
// Freezing the array ensures that it cannot be unintentionally modified.
const EMPTY_ARRAY = Object.freeze([])
const EMPTY_OBJECT = Object.freeze({})

const CONST = {
  EMPTY_ARRAY,
  EMPTY_OBJECT,
  APP_NAME_SHORT: "ARTMA",
  APP_NAME_LONG: "Automatic Replication Tools for Meta-Analysis",
  APP_DESCRIPTION:
    "A tool for researchers to automate the process of replication in meta-analysis",
  // APP_MOTTO: "Automate the process of replication in meta-analysis",
  APP_MOTTO: "Meta-analysis, at your fingertips",
  IMAGES: {
    ARTMA_LOGO: "/images/ARTMA_logo.png",
  },
  IMAGE_SIZE: {
    EXTRA_SMALL_IMAGE: {
      WIDTH: 24,
      HEIGHT: 24,
      SIZES: "24x24",
    },
    SMALL_IMAGE: {
      WIDTH: 36,
      HEIGHT: 36,
      SIZES: "36x36",
    },
    MEDIUM_IMAGE: {
      WIDTH: 48,
      HEIGHT: 48,
      SIZES: "48x48",
    },
    LARGE_IMAGE: {
      WIDTH: 64,
      HEIGHT: 64,
      SIZES: "64x64",
    },
    EXTRA_LARGE_IMAGE: {
      WIDTH: 96,
      HEIGHT: 96,
      SIZES: "96x96",
    },
  },
  THEME: {
    LIGHT: "light",
    DARK: "dark",
    PLACEHOLDER:
      "data:image/svg+xml;base64,PHN2ZyBzdHJva2U9IiNGRkZGRkYiIGZpbGw9IiNGRkZGRkYiIHN0cm9rZS13aWR0aD0iMCIgdmlld0JveD0iMCAwIDI0IDI0IiBoZWlnaHQ9IjIwMHB4IiB3aWR0aD0iMjAwcHgiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+PHJlY3Qgd2lkdGg9IjIwIiBoZWlnaHQ9IjIwIiB4PSIyIiB5PSIyIiBmaWxsPSJub25lIiBzdHJva2Utd2lkdGg9IjIiIHJ4PSIyIj48L3JlY3Q+PC9zdmc+Cg==",
  },
} as const

export default CONST
