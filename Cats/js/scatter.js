fetch('data/cats_reference.json')
  .then(response => {
    if (!response.ok) {
      throw new Error(`Failed to load data (${response.status})`);
    }
    return response.json();
  })
  .then(data => renderScatter(data))
  .catch(error => {
    d3.select('#scatter')
      .append('p')
      .attr('class', 'load-error')
      .text('Could not load scatter data. If you opened this file directly, run it through a local server instead.');
    console.error(error);
  });

function renderScatter(data) {
  data = data
    .map(d => ({
      tag_id: d.tag_id || 'Unknown',
      hrs_indoors: +d.hrs_indoors,
      age_years: +d.age_years
    }))
    .filter(d => Number.isFinite(d.hrs_indoors) && Number.isFinite(d.age_years));

  const container = d3.select('#scatter');
  const containerWidth = container.node().clientWidth || 900;

  const margin = { top: 48, right: 36, bottom: 64, left: 72 };
  const width = Math.max(560, containerWidth - 16) - margin.left - margin.right;
  const height = 520 - margin.top - margin.bottom;

  const svg = container
    .append('svg')
    .attr('viewBox', `0 0 ${width + margin.left + margin.right} ${height + margin.top + margin.bottom}`)
    .attr('preserveAspectRatio', 'xMidYMid meet');

  const defs = svg.append('defs');

  const pointGradient = defs
    .append('radialGradient')
    .attr('id', 'point-gradient')
    .attr('cx', '30%')
    .attr('cy', '30%');

  pointGradient.append('stop').attr('offset', '0%').attr('stop-color', '#d2f6ff');
  pointGradient.append('stop').attr('offset', '55%').attr('stop-color', '#4dc6f4');
  pointGradient.append('stop').attr('offset', '100%').attr('stop-color', '#0d7ea8');

  const glow = defs
    .append('filter')
    .attr('id', 'point-glow')
    .attr('x', '-50%')
    .attr('y', '-50%')
    .attr('width', '200%')
    .attr('height', '200%');

  glow.append('feGaussianBlur').attr('stdDeviation', 2.2).attr('result', 'blurred');
  glow.append('feMerge').selectAll('feMergeNode')
    .data(['blurred', 'SourceGraphic'])
    .enter()
    .append('feMergeNode')
    .attr('in', d => d);

  const chart = svg
    .append('g')
    .attr('transform', `translate(${margin.left},${margin.top})`);

  const x = d3.scaleLinear()
    .domain([0, d3.max(data, d => d.hrs_indoors) + 2])
    .nice()
    .range([0, width]);

  const y = d3.scaleLinear()
    .domain([0, d3.max(data, d => d.age_years) + 2])
    .nice()
    .range([height, 0]);

  const xAxis = d3.axisBottom(x).ticks(8).tickSize(-height).tickPadding(10);
  const yAxis = d3.axisLeft(y).ticks(7).tickSize(-width).tickPadding(10);

  chart
    .append('g')
    .attr('class', 'axis axis-x')
    .attr('transform', `translate(0,${height})`)
    .call(xAxis);

  chart
    .append('g')
    .attr('class', 'axis axis-y')
    .call(yAxis);

  chart.selectAll('.axis .tick line').attr('class', 'grid-line');
  chart.selectAll('.axis path').attr('class', 'axis-line');

  chart
    .append('text')
    .attr('class', 'axis-label')
    .attr('x', width / 2)
    .attr('y', height + 48)
    .attr('text-anchor', 'middle')
    .text('Hours Indoors');

  chart
    .append('text')
    .attr('class', 'axis-label')
    .attr('transform', 'rotate(-90)')
    .attr('x', -height / 2)
    .attr('y', -52)
    .attr('text-anchor', 'middle')
    .text('Age (Years)');

  const tooltip = d3.select('body')
    .append('div')
    .attr('class', 'tooltip')
    .style('opacity', 0);

  chart
    .selectAll('.dot')
    .data(data)
    .enter()
    .append('circle')
    .attr('class', 'dot')
    .attr('cx', d => x(d.hrs_indoors))
    .attr('cy', d => y(d.age_years))
    .attr('r', 0)
    .attr('fill', 'url(#point-gradient)')
    .attr('filter', 'url(#point-glow)')
    .on('mouseover', function(event, d) {
      d3.select(this)
        .transition()
        .duration(120)
        .attr('r', 11)
        .attr('stroke-width', 2.2);

      tooltip
        .interrupt()
        .transition()
        .duration(140)
        .style('opacity', 1);

      tooltip
        .html(`<strong>${d.tag_id}</strong><br>hrs_indoors: ${d.hrs_indoors}<br>age_years: ${d.age_years}`)
        .style('left', `${event.pageX + 14}px`)
        .style('top', `${event.pageY - 30}px`);
    })
    .on('mousemove', function(event) {
      tooltip
        .style('left', `${event.pageX + 14}px`)
        .style('top', `${event.pageY - 30}px`);
    })
    .on('mouseout', function() {
      d3.select(this)
        .transition()
        .duration(140)
        .attr('r', 7.5)
        .attr('stroke-width', 1.4);

      tooltip
        .interrupt()
        .transition()
        .duration(220)
        .style('opacity', 0);
    })
    .transition()
    .delay((_, i) => i * 28)
    .duration(460)
    .ease(d3.easeBackOut.overshoot(1.2))
    .attr('r', 7.5);
}
