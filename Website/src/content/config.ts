// 1. Import utilities from `astro:content`
import { defineCollection, z } from 'astro:content';
// 2. Define your collection(s)
const docsCollection = defineCollection({
    schema: z.object({
        title: z.string(),
        category: z.enum(['Getting Started', 'Basics', 'Concurrency', 'Effects'])
    })
});
// 3. Export a single `collections` object to register your collection(s)
//    This key should match your collection directory name in "src/content"
export const collections = {
  'docs': docsCollection,
};