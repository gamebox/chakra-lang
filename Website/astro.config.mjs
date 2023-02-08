import image from '@astrojs/image';
import tailwind from '@astrojs/tailwind';
import { defineConfig } from 'astro/config';
import path from 'path';

const grammarPath = `${path.resolve(process.cwd(), '../')}/Extensions/chakra-lang.chakra/syntaxes/chakra.tmGrammar.json`;

// https://astro.build/config
export default defineConfig({
    site: 'https://chakra-lang.dev',
    markdown: {
        shikiConfig: {
            langs: [
                {
                    id: 'chakra',
                    scopeName: 'source.chakra',
                    path: grammarPath
                }
            ]
        }
    },
    integrations: [
        tailwind(),
        image({
            serviceEntryPoint: '@astrojs/image/sharp'
        }),
    ],
    vite: {
        ssr: {
            external: ['svgo'],
        },
    },
});
