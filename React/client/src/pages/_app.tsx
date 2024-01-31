import "@/css/globals.scss"
import type { AppProps } from "next/app"
import RootLayout from "@/app/layout"

function MyApp({ Component, pageProps }: AppProps) {
  const { session, ...otherPageProps } = pageProps
  return (
    <RootLayout session={session}>
      <Component {...pageProps} />
    </RootLayout>
  )
}

export default MyApp
