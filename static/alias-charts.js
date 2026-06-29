(function () {
  var charts = {};
  var pollTimer = null;
  var tickStyle = { color: '#8b949e', font: { size: 9 } };
  var gridStyle = { color: '#21262d' };

  function hslaFill(color) {
    return color.replace(')', ', 0.15)').replace('hsl', 'hsla');
  }

  function chartOptions(color) {
    return {
      type: 'line',
      data: {
        labels: [],
        datasets: [{
          data: [],
          borderColor: color,
          backgroundColor: hslaFill(color),
          fill: true,
          borderWidth: 1.5,
          pointRadius: 0,
          tension: 0.3
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        animation: false,
        plugins: { legend: { display: false }, tooltip: { mode: 'index', intersect: false } },
        scales: {
          x: { display: true, ticks: { ...tickStyle, maxTicksLimit: 4 }, grid: gridStyle },
          y: { display: true, beginAtZero: true, ticks: { ...tickStyle, maxTicksLimit: 3, precision: 0 }, grid: gridStyle }
        },
        interaction: { mode: 'index', intersect: false }
      }
    };
  }

  function updateCharts(data) {
    if (!data || !data.aliases) return;
    data.aliases.forEach(function (a) {
      var id = 'chart-' + a.id;
      var el = document.getElementById(id);
      if (!el) return;
      if (!charts[id]) {
        charts[id] = new Chart(el, chartOptions(a.color));
      } else if (charts[id].canvas !== el) {
        charts[id].destroy();
        charts[id] = new Chart(el, chartOptions(a.color));
      }
      var ds = charts[id].data.datasets[0];
      charts[id].data.labels = data.labels;
      ds.data = a.counts;
      ds.borderColor = a.color;
      ds.backgroundColor = hslaFill(a.color);
      charts[id].update('none');
    });
  }

  function fetchCharts() {
    if (document.hidden) return;
    fetch('/ui/api/alias-charts')
      .then(function (r) { return r.json(); })
      .then(updateCharts)
      .catch(function () {});
  }

  window.fetchCharts = fetchCharts;

  function startPolling() {
    if (pollTimer) clearInterval(pollTimer);
    fetchCharts();
    pollTimer = setInterval(fetchCharts, window.CHART_POLL_MS || 5000);
  }

  document.addEventListener('visibilitychange', function () {
    if (!document.hidden) fetchCharts();
  });

  if (typeof Chart !== 'undefined') startPolling();
  else window.addEventListener('load', startPolling);
})();