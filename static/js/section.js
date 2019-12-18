function sparkline(elem, data, avg) {
    var data = data.sort((a, b) => (a.x == b.x) ? 0 : (a.x > b.x) ? 1 : -1).filter(d => d.y != -1);
    
    const WIDTH        = 200;
    const HEIGHT       = 60;
    const MARGIN       = { top: 4, right: 4, bottom: 4, left: 4 };
    const INNER_WIDTH  = WIDTH - MARGIN.left - MARGIN.right;
    const INNER_HEIGHT = HEIGHT - MARGIN.top - MARGIN.bottom;
    const DATA_COUNT   = data.length;
    const xmin = Math.min(...data.map(r => r.x));
    const xmax = Math.max(...data.map(r => r.x));
    const ymin = Math.min(...data.map(r => r.y));
    const ymax = Math.max(...data.map(r => r.y));
    const x    = d3.scaleLinear().domain([0, DATA_COUNT]).range([0, INNER_WIDTH]);
    const y    = d3.scaleLinear().domain([ymin, ymax]).range([INNER_HEIGHT, 0]);
    const svg = d3.select(elem).append('svg')
          .attr('viewBox', `0 0 ${WIDTH} ${HEIGHT}`)
          .append('g')
          .attr('transform', 'translate(' + MARGIN.left + ',' + MARGIN.top + ')');

    const line = d3.line()
          .x((d, i) => x(i))
          .y(d => y(d.y));
    svg.append('path').datum(data)
        .attr('fill', 'none')
        .attr('class', 'stroke-width-1 stroke-current text-gray-500')
        .attr('d', line);

    const prev = d3.line()
          .x((d, i) => x(i))
          .y(d => y(data[0].y));
    svg.append('path').datum(data)
        .attr('fill', 'none')
        .attr('class', 'stroke-width-1 stroke-current text-gray-400')
        .attr('d', prev);

    const avgl = d3.line()
          .x((d, i) => x(i))
          .y(d => y(avg));
    svg.append('path').datum(data)
        .attr('fill', 'none')
        .attr('class', 'stroke-width-1 stroke-current text-teal-500')
        .attr('stroke-dasharray', '2 3')
        .attr('d', avgl);

    svg.append('circle')
        .attr('cx', x(0))
        .attr('cy', y(data[0].y))
        .attr('class', 'r-2 fill-current text-gray-500');
    svg.append('circle')
        .attr('cx', x(DATA_COUNT - 1))
        .attr('cy', y(data[DATA_COUNT - 1].y))
        .attr('class', 'r-2 fill-current text-teal-500');
}

