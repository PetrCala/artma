import React, { useState, createContext } from "react"
import CONST from "@/CONST"

// Create a Context for the theme
export const ThemeContext = createContext({
  theme: CONST.THEME.DARK as string, // Default theme (later synch with database)
  toggleTheme: () => {}, // Placeholder function for toggling the theme
})

type ThemeProviderProps = {
  children: React.ReactNode
}

// Create a Provider Component
export const ThemeProvider = ({ children }: ThemeProviderProps) => {
  const [theme, setTheme] = useState(CONST.THEME.DARK as string) // Default to dark theme

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
