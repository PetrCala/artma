import { NextApiRequest, NextApiResponse } from "next";
import { getServerSession } from "next-auth/next";
import authOptions from "./auth/[...nextauth]";

type ApiResponse = {
  content?: string;
  error?: string;
};

export default async (req: NextApiRequest, res: NextApiResponse<ApiResponse>) => {
  const session = await getServerSession(req, res, authOptions);

  if (session) {
    res.send({
      content: "This is protected content. You can access this content because you are signed in.",
    });
  } else {
    res.status(401).send({
      error: "You must be signed in to view the protected content on this page.",
    });
  }
};
