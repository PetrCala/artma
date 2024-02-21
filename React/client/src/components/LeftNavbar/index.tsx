"use client"

// import CustomLink from "./custom-link"
// import { Button } from "../../ui/button"
import React, { Fragment } from "react"
import {
  NavigationMenu,
  NavigationMenuContent,
  NavigationMenuItem,
  NavigationMenuLink,
  NavigationMenuList,
  NavigationMenuTrigger,
  navigationMenuTriggerStyle,
} from "./NavigationMenu"
import ListItem from "./ListItem"
import Link from "next/link"
import { Menu, Transition } from "@headlessui/react"
import { GiHamburgerMenu } from "react-icons/gi"
import { cn } from "@src/libs/StylesUtils"

interface MyCustomButtonProps
  extends React.ButtonHTMLAttributes<HTMLButtonElement> {}

const MyCustomButton = React.forwardRef<HTMLButtonElement, MyCustomButtonProps>(
  ({ className, ...props }, ref) => {
    return (
      <button className={className} ref={ref} {...props}>
        <GiHamburgerMenu />
      </button>
    )
  }
)

interface MyLinkProps extends React.AnchorHTMLAttributes<HTMLAnchorElement> {
  href: string
  children: React.ReactNode
}

const MyLink = React.forwardRef<HTMLAnchorElement, MyLinkProps>(
  ({ className, ...props }, ref) => {
    const { href, children } = props
    return (
      <Link href={href} className={className} ref={ref}>
        {children}
      </Link>
    )
  }
)

const links = [
  { href: "/account-settings", label: "Account settings" },
  { href: "/support", label: "Support" },
  { href: "/license", label: "License" },
  { href: "/sign-out", label: "Sign out" },
]

function LeftNavbar() {
  return (
    <Menu>
      <Menu.Button as={MyCustomButton} />
      <Transition
        enter="transition duration-100 ease-out"
        enterFrom="transform scale-95 opacity-0"
        enterTo="transform scale-100 opacity-100"
        leave="transition duration-75 ease-out"
        leaveFrom="transform scale-100 opacity-100"
        leaveTo="transform scale-95 opacity-0"
      >
        <Menu.Items>
          {links.map((link) => (
            <MyLink
              key={link.href}
              href={link.href}
              className="ui-active:bg-blue-500 ui-active:text-white ui-not-active:bg-white ui-not-active:text-black"
            >
              {link.label}
            </MyLink>
          ))}
        </Menu.Items>
      </Transition>
    </Menu>
  )
}
LeftNavbar.displayName = "Left Navbar"

export default LeftNavbar

// // <div className="flex items-center space-x-2 lg:space-x-6">
//   {/* <CustomLink href="/"> */}
//   {/* <Button variant="ghost" className="p-0">
//       <Image src="/logo.png" alt="Home" width="32" height="32" />
//     </Button> */}
//   {/* </CustomLink> */}
//   {/* <NavigationMenu>
//     <NavigationMenuList>
//       <NavigationMenuItem>
//         <NavigationMenuTrigger>Server Side</NavigationMenuTrigger>
//         <NavigationMenuContent>
//           <ul className="grid gap-3 p-6 md:w-[400px] lg:w-[500px] lg:grid-cols-[.75fr_1fr]">
//             <ListItem href="/server-example" title="RSC Example">
//               Protecting React Server Component.
//             </ListItem>
//             <ListItem href="/middleware-example" title="Middleware Example">
//               Using Middleware to protect pages & APIs.
//             </ListItem>
//             <ListItem href="/api-example" title="Route Handler Example">
//               Getting the session inside an API Route.
//             </ListItem>
//           </ul>
//         </NavigationMenuContent>
//       </NavigationMenuItem>
//       <NavigationMenuItem>
//         <NavigationMenuLink
//           href="/client-example"
//           className={navigationMenuTriggerStyle()}
//         >
//           Client Side
//         </NavigationMenuLink>
//       </NavigationMenuItem>
//     </NavigationMenuList>
//   </NavigationMenu> */}
// // </div>
