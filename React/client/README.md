This is a [Next.js](https://nextjs.org/) project bootstrapped with [`create-next-app`](https://github.com/vercel/next.js/tree/canary/packages/create-next-app).

## Getting Started

First, run the development server:

```bash
npm run dev
# or
yarn dev
# or
pnpm dev
# or
bun dev
```

Open [http://localhost:3000](http://localhost:3000) with your browser to see the result.

You can start editing the page by modifying `app/page.tsx`. The page auto-updates as you edit the file.

This project uses [`next/font`](https://nextjs.org/docs/basic-features/font-optimization) to automatically optimize and load Inter, a custom Google Font.

## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

You can check out [the Next.js GitHub repository](https://github.com/vercel/next.js/) - your feedback and contributions are welcome!

## Deploy on Vercel

The easiest way to deploy your Next.js app is to use the [Vercel Platform](https://vercel.com/new?utm_medium=default-template&filter=next.js&utm_source=create-next-app&utm_campaign=create-next-app-readme) from the creators of Next.js.

Check out our [Next.js deployment documentation](https://nextjs.org/docs/deployment) for more details.

## Styling

### Themes

- `next-themes` is the module that provides the light/dark themes to the whole application. By default, there is no need to specify, for example, the background color, text color, etc. of different objects, as `next-themes` takes care of this under the hood.
- If you wish to specify custom colors, you may do so from the `css/colors.scss` module, which will make them available across the whole application.
- To specify custom colors that change with the theme, define these inside the `css/themes/<theme-module-name>.scss` names. These modules are used in the `globals.scss` module based on the current theme, making them available in the application. When adding a custom tag, in this way, make sure to add it to all existing themes, and under the same key. As an example, to add a variable `--image-border-color`, you would modify the theme files as follows.

  ```scss
  // light-theme.scss
  --image-border-color: rbg(<light-rgb-values>); // i.e., rgb(102, 255, 102)
  ```

  ```scss
  // dark-theme.scss
  --image-border-color: rbg(<dark-rgb-values>); // i.e., rgb(0, 51, 0)
  ```

### Multiple styles

- We use the `classnames` module to define multiple style object. You can do so by passing the `classNames` method to the `className` attribute. See the following example:

  ```tsx
  <p
    className={classNames(styles.themeOne, {
      [styles.themeTwo]: someVariable === 1,
    })}
  >
    Sample text
  </p>
  ```

### Icons

- For icons, we use the extensive [`react-icons` library](https://react-icons.github.io/react-icons/). Each icon there has instructions on how to add it to the project, so feel free to refer to those.
