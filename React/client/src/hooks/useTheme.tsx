// useTheme.js (or you can add this in ThemeContext.js)
import React, { useContext } from 'react';
import { ThemeContext } from '@/context/ThemeContext'; // Adjust the path as needed

const useTheme = () => {
  const context = useContext(ThemeContext);

  if (context === undefined) {
    throw new Error('useTheme must be used within a ThemeProvider');
  }

  return context;
};

export default useTheme;
