import React, { useState, createContext } from "react"
import CONST from "@/CONST"

// Create a Context for the theme
export const ThemeContext = createContext({
  theme: CONST.THEME.LIGHT as string, // Default theme
  toggleTheme: () => {}, // Placeholder function for toggling the theme
})

type ThemeProviderProps = {
  children: React.ReactNode
}

// Create a Provider Component
export const ThemeProvider = ({ children }: ThemeProviderProps) => {
  const [theme, setTheme] = useState(CONST.THEME.LIGHT as string) // Default to light theme

  // Function to toggle between dark and light themes
  const toggleTheme = () => {
    setTheme(theme === CONST.THEME.LIGHT ? CONST.THEME.DARK : CONST.THEME.LIGHT)
  }

  // Value provided to consumers
  const contextValue = {
    theme,
    toggleTheme,
  }

  return (
    <ThemeContext.Provider value={contextValue}>
      {children}
    </ThemeContext.Provider>
  )
}
