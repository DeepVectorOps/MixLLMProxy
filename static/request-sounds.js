(function () {
  var STORAGE = {
    enabled: 'mixllmproxy.sounds.enabled',
    volume: 'mixllmproxy.sounds.volume',
    events: 'mixllmproxy.sounds.events'
  };

  var EVENT_DEFS = [
    { name: 'new_request', tone: { freq: 880, duration: 0.06, type: 'triangle', gain: 0.7 } },
    { name: 'completed', tone: { freq: 660, freqEnd: 990, duration: 0.12, type: 'triangle', gain: 0.8 } },
    { name: 'error', tone: { freq: 220, duration: 0.15, type: 'square', gain: 0.9 } }
  ];

  var EVENT_NAMES = EVENT_DEFS.map(function (d) { return d.name; });
  var TONES = EVENT_DEFS.reduce(function (acc, d) {
    acc[d.name] = d.tone;
    return acc;
  }, {});

  var POLL_MS = window.REQUEST_SOUND_POLL_MS || 2000;
  var MIN_GAP_S = 0.08;

  var ctx = null;
  var pollTimer = null;
  var initialized = false;
  var known = Object.create(null);
  var playQueue = [];
  var playing = false;
  var nextPlayAt = 0;
  var syncing = false;

  function $(id) {
    return document.getElementById(id);
  }

  function defaultEvents() {
    return EVENT_NAMES.reduce(function (acc, name) {
      acc[name] = true;
      return acc;
    }, {});
  }

  function readBool(key, fallback) {
    var stored = localStorage.getItem(key);
    if (stored === null) return fallback;
    return stored === '1';
  }

  function isEnabled() {
    return readBool(STORAGE.enabled, true);
  }

  function setEnabled(on) {
    localStorage.setItem(STORAGE.enabled, on ? '1' : '0');
    syncControls();
    if (on) resumeContext();
  }

  function getEvents() {
    var raw = localStorage.getItem(STORAGE.events);
    if (!raw) return defaultEvents();
    try {
      var parsed = JSON.parse(raw);
      var events = defaultEvents();
      EVENT_NAMES.forEach(function (name) {
        if (typeof parsed[name] === 'boolean') events[name] = parsed[name];
      });
      return events;
    } catch (e) {
      return defaultEvents();
    }
  }

  function setEvents(events) {
    localStorage.setItem(STORAGE.events, JSON.stringify(events));
  }

  function isEventEnabled(name) {
    return !!getEvents()[name];
  }

  function allEventsEnabled(events) {
    return EVENT_NAMES.every(function (name) { return events[name]; });
  }

  function getVolume() {
    var v = parseFloat(localStorage.getItem(STORAGE.volume));
    if (isNaN(v)) return 0.25;
    return Math.max(0, Math.min(1, v));
  }

  function setVolume(v) {
    localStorage.setItem(STORAGE.volume, String(v));
  }

  function syncControls() {
    syncing = true;
    var enabled = isEnabled();
    var events = getEvents();

    if ($('sound-enabled')) $('sound-enabled').checked = enabled;
    if ($('sound-volume')) $('sound-volume').value = String(Math.round(getVolume() * 100));

    var badge = $('sound-status-badge');
    if (badge) {
      badge.textContent = enabled ? 'ON' : 'OFF';
      badge.className = 'badge ' + (enabled ? 'badge-green' : 'badge-red');
    }

    var filters = $('sound-event-filters');
    if (filters) filters.classList.toggle('is-muted', !enabled);

    EVENT_NAMES.forEach(function (name) {
      var box = $('sound-event-' + name);
      if (box) box.checked = events[name];
    });

    if ($('sound-event-all')) $('sound-event-all').checked = allEventsEnabled(events);
    syncing = false;
  }

  function getContext() {
    if (!ctx) {
      var Ctx = window.AudioContext || window.webkitAudioContext;
      if (!Ctx) return null;
      ctx = new Ctx();
    }
    return ctx;
  }

  function resumeContext() {
    var c = getContext();
    if (!c) return Promise.resolve();
    if (c.state === 'suspended') {
      return c.resume().then(function () {
        drainQueue();
      });
    }
    return Promise.resolve();
  }

  function scheduleTone(opts) {
    var item = {
      opts: opts,
      time: Date.now()
    };
    playQueue.push(item);
    if (playQueue.length > 5) {
      playQueue.shift();
    }
    drainQueue();
  }

  function drainQueue() {
    if (playing || !playQueue.length || !isEnabled()) return;
    var c = getContext();
    if (!c) return;

    if (c.state !== 'running') {
      // Discard stale tones to prevent a burst of sounds when resuming
      var nowTime = Date.now();
      playQueue = playQueue.filter(function (item) {
        return nowTime - item.time < 2000;
      });
      return;
    }

    var item = playQueue[0];
    if (Date.now() - item.time > 2000) {
      playQueue.shift();
      setTimeout(drainQueue, 0);
      return;
    }

    var now = c.currentTime;
    if (now < nextPlayAt) {
      setTimeout(drainQueue, (nextPlayAt - now) * 1000);
      return;
    }

    playing = true;
    playQueue.shift();
    playTone(item.opts);
    nextPlayAt = c.currentTime + MIN_GAP_S;
    setTimeout(function () {
      playing = false;
      drainQueue();
    }, MIN_GAP_S * 1000);
  }

  function playTone(opts) {
    var c = getContext();
    if (!c || c.state !== 'running') return;

    var t0 = c.currentTime;
    var duration = opts.duration || 0.08;
    var gainVal = getVolume() * (opts.gain || 1);
    var osc = c.createOscillator();
    var gain = c.createGain();

    var peakTime = Math.min(0.01, duration * 0.2);
    var holdTime = duration * 0.6;
    if (holdTime < peakTime) holdTime = peakTime;

    osc.type = opts.type || 'triangle';
    osc.frequency.setValueAtTime(opts.freq || 440, t0);
    if (opts.freqEnd) osc.frequency.linearRampToValueAtTime(opts.freqEnd, t0 + duration);

    gain.gain.setValueAtTime(0.0001, t0);
    gain.gain.exponentialRampToValueAtTime(Math.max(gainVal, 0.0001), t0 + peakTime);
    gain.gain.setValueAtTime(Math.max(gainVal, 0.0001), t0 + holdTime);
    gain.gain.exponentialRampToValueAtTime(0.0001, t0 + duration);

    osc.connect(gain);
    gain.connect(c.destination);
    osc.start(t0);
    osc.stop(t0 + duration + 0.02);

    osc.onended = function () {
      osc.disconnect();
      gain.disconnect();
    };
  }

  function playSound(name) {
    if (!isEnabled() || !isEventEnabled(name)) return;
    resumeContext().then(function () {
      if (TONES[name]) scheduleTone(TONES[name]);
    });
  }

  function classifyDone(status) {
    if (status == null) return null;
    return status >= 400 ? 'error' : 'completed';
  }

  function playForStatus(status) {
    if (status == null) playSound('new_request');
    else {
      var kind = classifyDone(status);
      if (kind) playSound(kind);
    }
  }

  function playTestSequence() {
    resumeContext().then(function () {
      var delay = 0;
      EVENT_NAMES.forEach(function (name) {
        if (!isEventEnabled(name)) return;
        setTimeout(function () { playSound(name); }, delay);
        delay += 180;
      });
    });
  }

  function diffEvent(row) {
    var prev = known[row.id];

    if (prev === undefined) {
      if (initialized) playForStatus(row.status);
      return;
    }

    if (prev == null && row.status != null && initialized) playForStatus(row.status);
  }

  function onFirstPage() {
    var page = parseInt(new URLSearchParams(window.location.search).get('page') || '1', 10);
    return page === 1;
  }

  function applySnapshot(rows) {
    var next = Object.create(null);
    var wasInitialized = initialized;
    var changed = false;

    rows.forEach(function (row) {
      if (wasInitialized) {
        var prev = known[row.id];
        if (prev === undefined || prev !== row.status) changed = true;
      }
      diffEvent(row);
      next[row.id] = row.status;
    });

    known = next;
    if (!initialized) initialized = true;
    return wasInitialized && changed;
  }

  function refreshPageData() {
    var url = window.location.href;
    fetch(url)
      .then(function (r) { return r.text(); })
      .then(function (html) {
        var parser = new DOMParser();
        var doc = parser.parseFromString(html, 'text/html');

        // 1. Replace requests table
        var newTable = doc.querySelector('table.requests');
        var oldTable = document.querySelector('table.requests');
        if (newTable && oldTable) {
          oldTable.parentNode.replaceChild(newTable, oldTable);
        }

        // 2. Replace pagination controls
        var newPaginations = doc.querySelectorAll('.pagination');
        var oldPaginations = document.querySelectorAll('.pagination');
        if (newPaginations.length === oldPaginations.length) {
          for (var i = 0; i < oldPaginations.length; i++) {
            oldPaginations[i].parentNode.replaceChild(newPaginations[i], oldPaginations[i]);
          }
        }

        // 3. Replace settings badges (Global Pause and Global Rate Limit)
        var newPause = doc.querySelector('.settings-card-pause');
        var oldPause = document.querySelector('.settings-card-pause');
        if (newPause && oldPause) {
          oldPause.parentNode.replaceChild(newPause, oldPause);
        }
        var newSpeed = doc.querySelector('.settings-card-speed');
        var oldSpeed = document.querySelector('.settings-card-speed');
        if (newSpeed && oldSpeed) {
          oldSpeed.parentNode.replaceChild(newSpeed, oldSpeed);
        }

        // 4. Replace rate limits section
        var newRateLimits = doc.querySelector('.rate-limits');
        var oldRateLimits = document.querySelector('.rate-limits');
        if (newRateLimits && oldRateLimits) {
          oldRateLimits.parentNode.replaceChild(newRateLimits, oldRateLimits);
          if (window.fetchCharts) window.fetchCharts();
        } else if (newRateLimits && !oldRateLimits) {
          var settingsRow = document.querySelector('.settings-row');
          if (settingsRow) {
            settingsRow.parentNode.insertBefore(newRateLimits, settingsRow);
            if (window.fetchCharts) window.fetchCharts();
          }
        } else if (!newRateLimits && oldRateLimits) {
          oldRateLimits.parentNode.removeChild(oldRateLimits);
        }
      })
      .catch(function (err) {
        console.error("Failed to refresh page data:", err);
      });
  }

  function fetchEvents() {
    if (document.hidden) return;
    fetch('/ui/api/request-events')
      .then(function (r) { return r.json(); })
      .then(function (data) {
        if (!data || !data.requests) return;
        if (applySnapshot(data.requests) && onFirstPage()) {
          refreshPageData();
        }
      })
      .catch(function () {});
  }

  function startPolling() {
    if (pollTimer) clearInterval(pollTimer);
    fetchEvents();
    pollTimer = setInterval(fetchEvents, POLL_MS);
  }

  function setAllEvents(on) {
    setEvents(EVENT_NAMES.reduce(function (acc, name) {
      acc[name] = on;
      return acc;
    }, {}));
    syncControls();
  }

  function setSingleEvent(name, on) {
    var events = getEvents();
    events[name] = on;
    setEvents(events);
    syncControls();
  }

  function bindControls() {
    syncControls();

    var master = $('sound-enabled');
    if (master) {
      master.addEventListener('change', function () {
        setEnabled(master.checked);
      });
    }

    var allBox = $('sound-event-all');
    if (allBox) {
      allBox.addEventListener('change', function () {
        if (!syncing) setAllEvents(allBox.checked);
      });
    }

    EVENT_NAMES.forEach(function (name) {
      var box = $('sound-event-' + name);
      if (!box) return;
      box.addEventListener('change', function () {
        if (!syncing) setSingleEvent(name, box.checked);
      });
    });

    var volume = $('sound-volume');
    if (volume) {
      volume.addEventListener('input', function () {
        setVolume(parseInt(volume.value, 10) / 100);
      });
    }

    var testBtn = $('sound-test');
    if (testBtn) {
      testBtn.addEventListener('click', function () {
        resumeContext().then(playTestSequence);
      });
    }

    var unlockEvents = ['click', 'keydown', 'mousedown', 'touchstart'];
    function unlock() {
      resumeContext();
      unlockEvents.forEach(function (e) {
        document.body.removeEventListener(e, unlock);
      });
    }
    unlockEvents.forEach(function (e) {
      document.body.addEventListener(e, unlock, { once: true });
    });
  }

  function init() {
    bindControls();
    startPolling();
  }

  document.addEventListener('visibilitychange', function () {
    if (!document.hidden) fetchEvents();
  });

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();