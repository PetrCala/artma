// Creating a default array and object this way because objects ({}) and arrays ([]) are not stable types.
// Freezing the array ensures that it cannot be unintentionally modified.
const EMPTY_ARRAY = Object.freeze([])
const EMPTY_OBJECT = Object.freeze({})

const CONST = {
  EMPTY_ARRAY,
  EMPTY_OBJECT,
  APP_NAME_SHORT: "ARTMA",
  APP_NAME_LONG: "Automatic Replication Tools for Meta-Analysis",
  IMAGES: {
    ARTMA_LOGO: "/images/ARTMA_logo.png",
    HOME: "/images/home.png",
  },
  THEME: {
    LIGHT: "light",
    DARK: "dark",
    PLACEHOLDER: "data:image/svg+xml;base64,PHN2ZyBzdHJva2U9IiNGRkZGRkYiIGZpbGw9IiNGRkZGRkYiIHN0cm9rZS13aWR0aD0iMCIgdmlld0JveD0iMCAwIDI0IDI0IiBoZWlnaHQ9IjIwMHB4IiB3aWR0aD0iMjAwcHgiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+PHJlY3Qgd2lkdGg9IjIwIiBoZWlnaHQ9IjIwIiB4PSIyIiB5PSIyIiBmaWxsPSJub25lIiBzdHJva2Utd2lkdGg9IjIiIHJ4PSIyIj48L3JlY3Q+PC9zdmc+Cg==",
  },
} as const

export default CONST
