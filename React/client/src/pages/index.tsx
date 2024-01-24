import { useSession } from "next-auth/react"
import MainPage from "@/app/mainPage";

export default function IndexPage() {
  // `session` will match the returned value of `callbacks.session()` from `NextAuth()`
  const { data: session } = useSession()

  return <MainPage />;
}