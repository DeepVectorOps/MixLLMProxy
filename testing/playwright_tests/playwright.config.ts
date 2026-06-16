import { defineConfig } from "@playwright/test";

const browserPath = process.env.PLAYWRIGHT_BROWSERS_PATH
  ? `${process.env.PLAYWRIGHT_BROWSERS_PATH}/chromium-1194/chrome-linux/chrome`
  : undefined;

export default defineConfig({
  testDir: ".",
  timeout: 30000,
  use: {
    baseURL: "http://localhost:8015",
    headless: true,
    ...(browserPath
      ? {
          launchOptions: { executablePath: browserPath },
        }
      : {}),
  },
});
