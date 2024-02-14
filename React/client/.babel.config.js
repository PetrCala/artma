module.exports = {
  presets: [
    // 'module:metro-react-native-babel-preset',
    ["@babel/preset-env", { targets: { node: "current" } }],
    "@babel/preset-typescript",
  ],
  plugins: [
    [
      "module-resolver",
      {
        extensions: [
          ".native.js",
          ".native.jsx",
          ".native.ts",
          ".native.tsx",
          ".js",
          ".jsx",
          ".ts",
          ".tsx",
        ],
        alias: {
          "@app": "./src/app",
          "@components": "./src/components",
          "@context": "./src/context",
          "@css": "./src/css",
          //   "@database": "./src/database",
          "@hooks": "./src/hooks",
          "@libs": "./src/libs",
          //   "@navigation": "./src/navigation",
          "@pages": "./src/pages",
          "@public": "./public",
          "@src": "./src",
          "@styles": "./src/styles",
          "@types": "./src/types",
          "@utils": "./src/utils",
        },
      },
    ],
  ],
}
