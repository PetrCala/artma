import type { IsEqual, ValueOf } from "type-fest"

const ROUTES = {
  HOME: "/",
  ABOUT: "/about",
  HELP: "/help",
  DATA: "/data",
  DATA_UPLOAD: "/data/upload",
  DATA_UPLOAD_CSV: "/data/upload/csv",
  DATA_UPLOAD_JSON: "/data/upload/json",
  DATA_UPLOAD_XML: "/data/upload/xml",
  DATA_UPLOAD_XLSX: "/data/upload/xlsx",
  DATA_UPLOAD_ZIP: "/data/upload/zip",
  DATA_UPLOAD_ZIP_CSV: "/data/upload/zip/csv",
  DATA_UPLOAD_ZIP_JSON: "/data/upload/zip/json",
  DATA_UPLOAD_ZIP_XML: "/data/upload/zip/xml",
  DATA_UPLOAD_ZIP_XLSX: "/data/upload/zip/xlsx",
  DATA_UPLOAD_ZIP_XLS: "/data/upload/zip/xls",
  DATA_UPLOAD_ZIP_XLSM: "/data/upload/zip/xlsm",
  DATA_UPLOAD_ZIP_ODS: "/data/upload/zip/ods",
  DATA_UPLOAD_ZIP_ODT: "/data/upload/zip/odt",
  DATA_VIEW: "/data/view",
  USER_VIEW: {
    route: "/user/:uid/view",
    getRoute: (uid: string) => `/user/${uid}/view`,
  },
  MODELS: "/models",
  EXPLORATION: "/exploration",
  SETTINGS: "/settings",
} as const

export default ROUTES

type ExtractRouteName<TRoute> = TRoute extends {
  getRoute: (...args: any[]) => infer TRouteName
}
  ? TRouteName
  : TRoute

type AllRoutes = {
  [K in keyof typeof ROUTES]: ExtractRouteName<(typeof ROUTES)[K]>
}[keyof typeof ROUTES]

type RouteIsPlainString = IsEqual<AllRoutes, string>

type Route = RouteIsPlainString extends true ? never : AllRoutes

export type { Route }
