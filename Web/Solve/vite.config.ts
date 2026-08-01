import { defineConfig } from 'vite'

// https://vitejs.dev/config/
export default defineConfig({
    clearScreen: false,
    base: './',   // GitHub Pages serves the site from a subdirectory
    server: {
        watch: {
            ignored: [
                "**/*.fs",             // Don't watch F# files
                "**/fable_modules/**"  // Fable's package cache changes only when packages do
            ]
        }
    },
    build: {
        outDir: '../docs',   // published to GitHub Pages
        emptyOutDir: true,
        assetsDir: '',
        rollupOptions: {
            output: {
                // stable names, so each publish is a clean diff
                entryFileNames: 'index.js',
                assetFileNames: 'index.[ext]'
            }
        }
    }
})
