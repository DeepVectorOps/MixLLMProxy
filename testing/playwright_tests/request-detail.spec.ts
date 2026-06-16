import { test, expect } from "@playwright/test";

test.describe("Request detail page layout", () => {
  test("body panels render side-by-side without overflow", async ({ page }) => {
    await page.goto("/ui/");

    // Find a request row that has actual content in both bodies (not just "-")
    await page.waitForSelector("table.requests tbody tr.req-row", { timeout: 10000 });
    const rows = page.locator("table.requests tbody tr.req-row");

    // Find first row with non-dash request AND response body preview (columns 11 and 12)
    let targetHref: string | null = null;
    const rowCount = await rows.count();
    for (let i = 0; i < rowCount; i++) {
      const row = rows.nth(i);
      const reqCell = row.locator("td.req");
      const respCell = row.locator("td.resp");
      const reqText = (await reqCell.textContent()) || "";
      const respText = (await respCell.textContent()) || "";
      if (reqText.trim() !== "-" && respText.trim() !== "-") {
        targetHref = await row.getAttribute("data-href");
        break;
      }
    }

    if (!targetHref) {
      test.skip(true, "No request found with both body and response content");
      return;
    }

    await page.goto(targetHref);

    // Wait for the body panels to be rendered
    await page.waitForSelector("#req-body", { timeout: 10000 });
    await page.waitForSelector("#resp-body", { timeout: 10000 });

    // Wait for json-formatter-js to finish rendering (it clears textContent first)
    await page.waitForFunction(
      () => {
        const reqEl = document.querySelector("#req-body");
        const respEl = document.querySelector("#resp-body");
        if (!reqEl || !respEl) return false;
        // The JS clears textContent then appends formatter DOM; textContent="" means it fired
        // But if raw was empty, textContent stays "(none)"
        const reqDone = reqEl.textContent === "(none)" || reqEl.children.length > 0;
        const respDone = respEl.textContent === "(none)" || respEl.children.length > 0;
        return reqDone && respDone;
      },
      { timeout: 15000 }
    );

    const bodiesGrid = page.locator(".bodies");
    await expect(bodiesGrid).toBeVisible();

    const bodyCols = bodiesGrid.locator(".body-col");
    const colCount = await bodyCols.count();
    expect(colCount).toBe(2);

    // ---- CHECK 1: columns are side-by-side (not stacked) ----
    const firstColBox = await bodyCols.nth(0).boundingBox();
    const secondColBox = await bodyCols.nth(1).boundingBox();
    expect(firstColBox).not.toBeNull();
    expect(secondColBox).not.toBeNull();

    // second column should start roughly where first column ends (within gap tolerance)
    // They are side-by-side, so second.left >= first.right - a few px gap
    const gap = (secondColBox!.x) - (firstColBox!.x + firstColBox!.width);
    expect(gap).toBeGreaterThanOrEqual(0);
    expect(gap).toBeLessThan(64); // 16px gap + some tolerance

    // ---- CHECK 2: no horizontal scrollbar on body panels ----
    const bodyPanels = page.locator(".body");
    const panelCount = await bodyPanels.count();
    for (let i = 0; i < panelCount; i++) {
      const panel = bodyPanels.nth(i);
      const elId = await panel.getAttribute("id");
      const hasHScroll = await panel.evaluate((el) => el.scrollWidth > el.clientWidth + 2);
      expect(
        hasHScroll,
        `${elId || "body panel " + i}: has horizontal overflow (scrollWidth=${await panel.evaluate((el) => el.scrollWidth)}, clientWidth=${await panel.evaluate((el) => el.clientWidth)})`
      ).toBe(false);
    }

    // ---- CHECK 3: no body-col content is clipped (overflow: hidden should not clip visible content) ----
    for (let i = 0; i < colCount; i++) {
      const col = bodyCols.nth(i);
      const colChildren = col.locator("> *");
      const childCount = await colChildren.count();
      for (let j = 0; j < childCount; j++) {
        const child = colChildren.nth(j);
        const childBox = await child.boundingBox();
        if (!childBox) continue;
        const colBox = await col.boundingBox();
        if (!colBox) continue;
        // child right edge should not exceed col right edge by more than 2px
        const overflow = childBox.x + childBox.width - (colBox.x + colBox.width);
        expect(
          overflow,
          `.body-col ${i} child ${j} overflows column by ${overflow}px`
        ).toBeLessThanOrEqual(2);
      }
    }

    // ---- CHECK 4: body panels are roughly equal width (within 10px) ----
    const reqBodyBox = await page.locator("#req-body").boundingBox();
    const respBodyBox = await page.locator("#resp-body").boundingBox();
    if (reqBodyBox && respBodyBox) {
      const widthDiff = Math.abs(reqBodyBox.width - respBodyBox.width);
      expect(widthDiff, `Body panel widths differ by ${widthDiff}px`).toBeLessThanOrEqual(10);
    }
  });
});
