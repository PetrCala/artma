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
  ATTACHMENT_FILE_TYPE: {
    FILE: "file",
    IMAGE: "image",
    VIDEO: "video",
  },
  BROWSER: {
    CHROME: "chrome",
    FIREFOX: "firefox",
    IE: "ie",
    EDGE: "edge",
    Opera: "opera",
    SAFARI: "safari",
    OTHER: "other",
  },
  BUTTON_STATES: {
    DEFAULT: "default",
    ACTIVE: "active",
    PRESSED: "pressed",
    COMPLETE: "complete",
    DISABLED: "disabled",
  },
  DATE: {
    SQL_DATE_TIME: "YYYY-MM-DD HH:mm:ss",
    FNS_FORMAT_STRING: "yyyy-MM-dd",
    LOCAL_TIME_FORMAT: "h:mm a",
    YEAR_MONTH_FORMAT: "yyyyMM",
    MONTH_FORMAT: "MMMM",
    WEEKDAY_TIME_FORMAT: "eeee",
    MONTH_DAY_ABBR_FORMAT: "MMM d",
    SHORT_DATE_FORMAT: "MM-dd",
    MONTH_DAY_YEAR_ABBR_FORMAT: "MMM d, yyyy",
    MONTH_DAY_YEAR_FORMAT: "MMMM d, yyyy",
    FNS_TIMEZONE_FORMAT_STRING: "yyyy-MM-dd'T'HH:mm:ssXXX",
    FNS_DB_FORMAT_STRING: "yyyy-MM-dd HH:mm:ss.SSS",
    LONG_DATE_FORMAT_WITH_WEEKDAY: "eeee, MMMM d, yyyy",
    UNIX_EPOCH: "1970-01-01 00:00:00.000",
    MAX_DATE: "9999-12-31",
    MIN_DATE: "0001-01-01",
    ORDINAL_DAY_OF_MONTH: "do",
  },
  ENVIRONMENT: {
    DEV: "development",
    STAGING: "staging",
    PRODUCTION: "production",
  },
  IMAGES: {
    ARTMA_LOGO: "/images/ARTMA_logo.png",
  },
  INPUT_MODE: {
    NONE: "none",
    TEXT: "text",
    DECIMAL: "decimal",
    NUMERIC: "numeric",
    TEL: "tel",
    SEARCH: "search",
    EMAIL: "email",
    URL: "url",
  },
  // PDF_VIEWER_URL: '/pdf/web/viewer.html',
  //  TERMS_URL: `${USE_SOME_URL}/terms`,
  //   PRIVACY_URL: `${USE_SOME_URL}/privacy`,
  SIGN_IN_METHOD: {
    // APPLE: "Apple",
    // GOOGLE: "Google",
  },
  THEME: {
    DEFAULT: "system",
    LIGHT: "light",
    DARK: "dark",
    PLACEHOLDER:
      "data:image/svg+xml;base64,PHN2ZyBzdHJva2U9IiNGRkZGRkYiIGZpbGw9IiNGRkZGRkYiIHN0cm9rZS13aWR0aD0iMCIgdmlld0JveD0iMCAwIDI0IDI0IiBoZWlnaHQ9IjIwMHB4IiB3aWR0aD0iMjAwcHgiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+PHJlY3Qgd2lkdGg9IjIwIiBoZWlnaHQ9IjIwIiB4PSIyIiB5PSIyIiBmaWxsPSJub25lIiBzdHJva2Utd2lkdGg9IjIiIHJ4PSIyIj48L3JlY3Q+PC9zdmc+Cg==",
  },
  TIME_PERIOD: {
    AM: "AM",
    PM: "PM",
  },
} as const

export default CONST
