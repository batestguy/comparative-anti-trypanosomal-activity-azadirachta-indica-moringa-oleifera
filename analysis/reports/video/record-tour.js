#!/usr/bin/env node
/**
 * Records the trypanosoma report tour (~2:45).
 *
 *   python -m http.server 8001                      # from the repo root
 *   node analysis/reports/video/record-tour.js      # → out/video/raw.webm
 *
 * Then mux the music bed and compress with ffmpeg (see analysis/reports/video/README.md).
 *
 * The page is a purpose-built tour of the committed results: every figure is the
 * PNG in analysis/results/figures and every number on screen is read from
 * analysis/results/tables. Nothing is simulated data.
 */
'use strict';

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
  throw new Error('playwright not found — set NODE_PATH to a node_modules containing it');
}

const { chromium } = loadPlaywright();

const TOUR_URL = process.env.TOUR_URL || 'http://127.0.0.1:8001/analysis/reports/video/tour.html';
const OUT_DIR = path.resolve(__dirname, 'out', 'video');
const SCENE_H = 720;

let page;
let T0 = 0;
const mark = (label) => console.log(label, ((Date.now() - T0) / 1000).toFixed(2) + 's');

/* ------------------------------------------------------------------ cards */

const CARD_CSS = `
  * { box-sizing: border-box; }
  html, body { margin: 0; height: 100%; }
  body {
    font-family: system-ui, -apple-system, "Segoe UI", sans-serif;
    background: #f7f3e8; color: #0d2a21;
    display: flex; align-items: center; justify-content: center; overflow: hidden;
  }
  body::before {
    content: ""; position: fixed; top: 0; left: 0; right: 0; height: 8px;
    background: repeating-linear-gradient(90deg,#0d3d2d 0 26px,#c9a227 26px 40px);
  }
  body::after {
    content: ""; position: fixed; inset: 0; pointer-events: none; opacity: .45;
    background-image: repeating-linear-gradient(135deg, rgba(13,61,45,.05) 0 2px, transparent 2px 28px);
  }
  .card { position: relative; z-index: 1; max-width: 1010px; padding: 0 64px; }
  .eyebrow {
    font-size: 12px; letter-spacing: .19em; text-transform: uppercase;
    color: #a8791a; font-weight: 700; margin: 0 0 22px;
  }
  h1 {
    font-family: "Iowan Old Style", "Palatino Linotype", Palatino, Georgia, serif;
    font-weight: 600; font-size: 68px; line-height: 1.05; margin: 0 0 22px; letter-spacing: -.02em;
  }
  h1 em { font-style: italic; color: #0a4d36; }
  p.lede { font-size: 21px; line-height: 1.5; margin: 0 0 26px; max-width: 820px; color: #2c4d40; }
  p.foot { font-size: 13px; color: #6b8377; margin: 0; }
  .rule { height: 2px; width: 76px; background: #c9a227; margin: 26px 0 22px; }
  .stats { display: flex; gap: 44px; margin: 26px 0 0; }
  .stats b {
    display: block; font-family: "Iowan Old Style", Palatino, Georgia, serif;
    font-size: 38px; font-weight: 600; line-height: 1;
  }
  .stats span { font-size: 11px; letter-spacing: .07em; text-transform: uppercase; color: #6b8377; }
  .url { font-family: ui-monospace, Consolas, monospace; font-size: 14.5px; color: #0a4d36;
    line-height: 1.55; word-break: break-word; max-width: 860px; }
  .reveal { opacity: 0; transform: translateY(14px); animation: rise .9s cubic-bezier(.22,.61,.36,1) forwards; }
  .d1 { animation-delay: .25s } .d2 { animation-delay: .6s } .d3 { animation-delay: 1s } .d4 { animation-delay: 1.4s }
  @keyframes rise { to { opacity: 1; transform: none; } }
`;

function cardHtml(kind) {
  const head = `<!doctype html><html><head><meta charset="utf-8"><style>${CARD_CSS}</style></head><body>`;

  if (kind === 'intro') {
    return head + `<div class="card">
  <p class="eyebrow reveal d1">Statistical report — recorded walkthrough</p>
  <h1 class="reveal d1">Two plants.<br><em>Two parasites.</em></h1>
  <p class="lede reveal d2">Methanolic extracts of Azadirachta indica and Moringa oleifera
    against Trypanosoma congolense and T. evansi — sixteen blocks, from a messy workbook
    to a rank-based analysis and a thesis-ready chapter.</p>
  <div class="stats reveal d3">
    <div><b>16</b><span>blocks</span></div>
    <div><b>8</b><span>per plant</span></div>
    <div><b>4,480</b><span>parasitaemia readings</span></div>
    <div><b>0</b><span>machine learning</span></div>
  </div>
  <div class="rule reveal d4"></div>
  <p class="foot reveal d4">Every figure and number below is read from the committed results ·
    Music: “Headlock”, Imogen Heap</p>
</div></body></html>`;
  }

  return head + `<div class="card">
  <p class="eyebrow reveal d1">Reproduce it</p>
  <h1 class="reveal d1">Workbook to Word,<br><em>four commands.</em></h1>
  <p class="lede reveal d2">Parse, analyse, render. Structural guards fail fast if the
    sheet layout ever drifts, so the report cannot quietly disagree with the data.</p>
  <p class="url reveal d3">github.com/batestguy/comparative-anti-trypanosomal-activity-azadirachta-indica-moringa-oleifera</p>
  <div class="rule reveal d4"></div>
  <p class="foot reveal d4">© 2026 Jerry Zachary Bannister · CC BY 4.0 · R 4.5.2 + Quarto 1.9</p>
</div></body></html>`;
}

/* --------------------------------------------------------------- overlays */

const OVERLAY_CSS = `
#demo-caption {
  position: fixed; left: 50%; bottom: 74px; transform: translateX(-50%) translateY(6px);
  z-index: 99998; max-width: min(86vw, 760px); padding: 10px 20px 11px;
  background: rgba(9,40,30,.93); color: #f4efe2; border: 1px solid rgba(201,162,39,.5);
  border-radius: 999px; box-shadow: 0 10px 34px rgba(0,0,0,.24);
  font: 500 15px/1.35 system-ui, -apple-system, "Segoe UI", sans-serif; letter-spacing: .005em;
  text-align: center; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
  opacity: 0; transition: opacity .3s ease, transform .3s ease; pointer-events: none;
}
#demo-caption.on { opacity: 1; transform: translateX(-50%) translateY(0); }
#demo-cursor {
  position: fixed; left: 0; top: 0; z-index: 99999; width: 24px; height: 24px;
  pointer-events: none; filter: drop-shadow(0 2px 4px rgba(0,0,0,.35));
  transition: transform .42s cubic-bezier(.22,.61,.36,1);
  transform: translate3d(-80px,-80px,0);
}
.demo-ripple {
  position: fixed; z-index: 99997; width: 16px; height: 16px; margin: -8px 0 0 -8px;
  border: 2px solid rgba(13,61,45,.8); border-radius: 50%; pointer-events: none;
  animation: demo-rip .62s ease-out forwards;
}
@keyframes demo-rip {
  from { transform: scale(.4); opacity: .95; }
  to   { transform: scale(3.4); opacity: 0; }
}
`;

async function installOverlays(page) {
  await page.evaluate((css) => {
    if (document.getElementById('demo-cursor')) return;
    const st = document.createElement('style');
    st.textContent = css;
    document.head.appendChild(st);

    const cur = document.createElement('div');
    cur.id = 'demo-cursor';
    cur.innerHTML = '<svg width="24" height="24" viewBox="0 0 24 24">' +
      '<path d="M4.5 2.2 L19.6 11.8 L12.7 13.3 L16.9 20.7 L14.2 22.2 L10 14.8 L4.5 19.4 Z" ' +
      'fill="#ffffff" stroke="#0a2f22" stroke-width="1.5" stroke-linejoin="round"/></svg>';

    const cap = document.createElement('div');
    cap.id = 'demo-caption';

    document.body.appendChild(cur);
    document.body.appendChild(cap);

    window.__demoCursor = (x, y) => { cur.style.transform = 'translate3d(' + (x - 3) + 'px,' + (y - 2) + 'px,0)'; };
    window.__demoRipple = (x, y) => {
      const r = document.createElement('div');
      r.className = 'demo-ripple';
      r.style.left = x + 'px';
      r.style.top = y + 'px';
      document.body.appendChild(r);
      setTimeout(() => r.remove(), 800);
    };
    window.__demoCap = (text) => {
      if (!text) { cap.classList.remove('on'); return; }
      cap.textContent = text;
      cap.classList.add('on');
    };
  }, OVERLAY_CSS);
}

/* ---------------------------------------------------------------- helpers */

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function moveTo(x, y, pause = 420) {
  await page.evaluate(([x, y]) => window.__demoCursor(x, y), [x, y]);
  await page.mouse.move(x, y);
  await sleep(pause);
}

async function clickAt(x, y, pause = 800) {
  await moveTo(x, y, 380);
  await page.evaluate(([x, y]) => window.__demoRipple(x, y), [x, y]);
  await page.mouse.down();
  await sleep(80);
  await page.mouse.up();
  await sleep(pause);
}

async function boxOf(sel) {
  const b = await page.locator(sel).first().boundingBox();
  if (!b) throw new Error('no bounding box for ' + sel);
  return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
}

async function clickSel(sel, pause = 800) {
  const c = await boxOf(sel);
  await clickAt(c.x, c.y, pause);
}

async function hoverSel(sel, pause = 1200) {
  const c = await boxOf(sel);
  await moveTo(c.x, c.y, 320);
  await page.mouse.move(c.x + 1, c.y + 1);
  await page.mouse.move(c.x, c.y);
  await sleep(pause);
}

async function caption(text) {
  await page.evaluate((t) => window.__demoCap(t), text);
}

/** Smooth scroll to a y offset, stepping like a real wheel/trackpad. */
async function scrollTo(y, ms = 760) {
  const from = await page.evaluate(() => window.scrollY);
  const steps = 14;
  for (let i = 1; i <= steps; i++) {
    const v = Math.round(from + ((y - from) * i) / steps);
    await page.evaluate((v) => window.scrollTo(0, v), v);
    await page.mouse.move(640, 460);
    await sleep(ms / steps);
  }
}

const scene = (i) => scrollTo(i * SCENE_H);

/* ------------------------------------------------------------------- main */

(async () => {
  fs.mkdirSync(OUT_DIR, { recursive: true });
  const titlePath = path.join(OUT_DIR, 'title.html');
  const endPath = path.join(OUT_DIR, 'end.html');
  fs.writeFileSync(titlePath, cardHtml('intro'));
  fs.writeFileSync(endPath, cardHtml('end'));

  const browser = await chromium.launch({ channel: 'chrome' });
  const context = await browser.newContext({
    viewport: { width: 1280, height: 720 },
    deviceScaleFactor: 1,
    recordVideo: { dir: OUT_DIR, size: { width: 1280, height: 720 } },
  });

  T0 = Date.now();
  page = await context.newPage();

  // ---------- title ----------
  await page.goto('file:///' + titlePath.replace(/\\/g, '/'), { waitUntil: 'networkidle' });
  await sleep(9200);

  await page.goto(TOUR_URL, { waitUntil: 'networkidle', timeout: 60000 });
  await page.waitForTimeout(500);
  await installOverlays(page);

  // 01 design
  mark('scene1');
  await caption('Sixteen blocks: two plants × four parts × two parasites');
  await hoverSel('.cell:nth-child(3)', 1600);
  await sleep(9200);

  // 02 parsing
  mark('scene2');
  await scene(1);
  await caption('The workbook fights back: merged labels, an offset panel, 42 capped readings');
  await hoverSel('.log div:first-child', 1500);
  await sleep(9400);

  // 03 phytochemistry
  mark('scene3');
  await scene(2);
  await caption('Phytochemistry: richness follows the plant part, not the species');
  await sleep(6800);
  await caption('Click the figure to read the presence matrix');
  await clickSel('.scene[data-scene="3"] figure img', 2400);
  await caption('Better as a matrix: tannins in every part except seeds, in both plants');
  await sleep(3400);
  mark('lightbox-close');
  await clickAt(120, 660, 900);   // dismiss the lightbox on the backdrop

  // 04 motility
  mark('scene4');
  await scene(3);
  await caption('In vitro: motility collapses in strict dose order, both parasites');
  await sleep(10400);

  // 05 TTI
  mark('scene5');
  await scene(4);
  await caption('Median time to immobilisation: 15 minutes at the top dose, censored at the bottom');
  await sleep(10600);

  // 06 AUC
  mark('scene6');
  await scene(5);
  await caption('In vivo: cumulative burden falls with dose — T. evansi p = 0.006, T. congolense p = 0.074');
  await sleep(10800);

  // 07 peak
  mark('scene7');
  await scene(6);
  await caption('Peak parasitaemia lands on exactly zero at 100 mg/ml');
  await sleep(9800);

  // 08 mortality
  mark('scene8');
  await scene(7);
  await caption('And no deaths at the effective dose; controls die in every block');
  await sleep(9800);

  // 09 superiority
  mark('scene9');
  await scene(8);
  await caption('Superiority: eight paired differences split both ways — p = 1.0');
  await hoverSel('.scene[data-scene="9"] figure img', 1400);
  await sleep(9200);

  // 10 weight and PCV
  mark('scene10');
  await scene(9);
  await caption('Weight and PCV: no treatment signal, but T. congolense is the more pathogenic');
  await sleep(10200);

  // 11 robustness
  mark('scene11');
  await scene(10);
  await caption('Leave-one-block-out: every headline claim holds in 8 of 8 refits');
  await hoverSel('pre', 1500);
  await sleep(9600);
  await caption('');
  await sleep(400);

  // ---------- end card ----------
  mark('end');
  await page.goto('file:///' + endPath.replace(/\\/g, '/'), { waitUntil: 'networkidle' });
  await sleep(6000);

  mark('stop');
  const video = page.video();
  await context.close();
  await browser.close();

  const src = await video.path();
  const dest = path.join(OUT_DIR, 'raw.webm');
  fs.copyFileSync(src, dest);
  console.log('recorded:', dest);
})().catch((err) => {
  console.error(err);
  process.exit(1);
});
