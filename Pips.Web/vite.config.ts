import { defineConfig } from 'vite'

// https://vitejs.dev/config/
export default defineConfig({
    clearScreen: false,
    server: {
        watch: {
            ignored: [
                "**/*.fs",             // Don't watch F# files
                "**/fable_modules/**"  // Fable's package cache changes only when packages do
            ]
        }
    }
})