import { useSession } from "next-auth/react";

const MainPage = () => {
  const { data: session } = useSession();

  // if (session) {
  //   return <div>Welcome, {session.user.name}</div>;
  // }

  // return <div>You are not signed in</div>;
  return <div>Welcome, user!</div>;
};

export default MainPage;