import type { IsEqual, ValueOf } from "type-fest"

const ROUTES = {
  HOME: "/",
  ABOUT: "/about",
  HELP: "/help",
  DATA: "/data",
  DATA_UPLOAD: "/data/upload",
  DATA_STORAGE: "/data/storage",
  USER_VIEW: {
    route: "/user/:uid/view",
    getRoute: (uid: string) => `/user/${uid}/view` as const,
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

/**
 * Represents all routes in the app as a union of literal strings.
 *
 * If this type resolves to `never`, it implies that one or more routes defined within `ROUTES` have not correctly used
 * `as const` in their `getRoute` function return value.
 */
type Route = RouteIsPlainString extends true ? never : AllRoutes

export type { Route }
