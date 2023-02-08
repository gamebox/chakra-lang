const defaultTheme = require("tailwindcss/defaultTheme");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.{astro,html,js,jsx,svelte,ts,tsx,vue}"],
  theme: {
    extend: {
      fontFamily: {
        sans: ["InterVariable", ...defaultTheme.fontFamily.sans],
      },
      colors: {
        primary: "var(--color-primary)",
        secondary: "var(--color-secondary)",
      },
      textColor: {
        default: "var(--color-text)",
        offset: "var(--color-text-offset)",
      },
      backgroundColor: {
        default: "var(--color-background)",
        offset: "var(--color-background-offset)",
      },
      borderColor: {
        default: "var(--color-border)",
      },
      typography: ({ theme }) => ({
        pink: {
          css: {
            '--tw-prose-body': 'var(--color-text)',
          }
        }
      }),
    },
  },
  corePlugins: {
    fontSize: false,
  },
  plugins: [
    require("tailwindcss-fluid-type"),
    require("@tailwindcss/typography")
  ],
};