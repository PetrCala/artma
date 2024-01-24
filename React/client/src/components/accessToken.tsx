import { useSession } from "next-auth/react"

export default function Component() {
  const { data: session } = useSession();

  if (session) {
    return <div>Access Token: {session.accessToken}</div>;
  } else {
    return <div>You are not signed in.</div>;
  }
}
