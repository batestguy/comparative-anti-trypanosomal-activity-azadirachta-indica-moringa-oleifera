// Snapshot each scene of tour.html at 1280x720 for layout review.
const path = require('path');
const fs = require('fs');

function loadPlaywright() {
  for (const name of ['playwright', 'playwright-core']) {
    try { return require(name); } catch (e) { /* next */ }
  }
  const roots = [
    process.env.APPDATA && path.join(process.env.APPDATA, 'npm', 'node_modules'),
    (() => { try { return require('child_process').execSync('npm root -g').toString().trim(); } catch (e) { return ''; } })(),
  ].filter(Boolean);
  for (const root of roots) {
    for (const sub of ['@playwright/mcp/node_modules/playwright-core', 'playwright-core', 'playwright']) {
      try { return require(path.join(root, sub)); } catch (e) { /* next */ }
    }
  }
  throw new Error('playwright not found');
}

const { chromium } = loadPlaywright();

(async () => {
  const out = path.resolve(__dirname, 'out', 'scenes');
  fs.mkdirSync(out, { recursive: true });
  const browser = await chromium.launch({ channel: 'chrome' });
  const page = await browser.newPage({ viewport: { width: 1280, height: 720 } });
  await page.goto('http://127.0.0.1:8001/analysis/reports/video/tour.html', { waitUntil: 'networkidle' });
  const n = await page.locator('.scene').count();
  for (let i = 0; i < n; i++) {
    await page.evaluate((i) => window.scrollTo(0, i * 720), i);
    await page.waitForTimeout(350);
    await page.screenshot({ path: path.join(out, `scene-${String(i + 1).padStart(2, '0')}.png`) });
  }
  // overflow report
  const issues = await page.evaluate(() => {
    const out = [];
    document.querySelectorAll('.scene').forEach((s, i) => {
      if (s.scrollHeight > s.clientHeight + 2) out.push({ scene: i + 1, scrollHeight: s.scrollHeight, clientHeight: s.clientHeight });
    });
    return out;
  });
  console.log('scenes:', n, 'overflow:', JSON.stringify(issues));
  await browser.close();
})();
