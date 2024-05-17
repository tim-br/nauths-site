module.exports = {
  content: [
    "../index.html",
    "../web-content/**/*.html",  // Adjusted to include all HTML files in web-content
    "../templates/**/*.html",   // Assuming templates directory is at the same level as web
    // Include any other paths where your HTML or content that uses Tailwind classes might live
  ],
  theme: {
    extend: {},
  },
  plugins: [],
}