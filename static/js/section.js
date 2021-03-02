function sparkline(elem, data, avg) {
    var data = data.sort((a, b) => (a.x == b.x) ? 0 : (a.x < b.x) ? 1 : -1).filter(d => d.y != -1);
    
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

    if (data.length <= 1) {
        svg.append("text")
            .attr("transform", `translate(-3, ${HEIGHT / 2})`)
            .attr('class', 'stroke-width-1 stroke-current text-gray-500')
            .style("stroke-width", "0.35px")
            .style("font-size", "0.45em")
            .style("font-family", "Recursive Mono")
            .style("text-anchor", "start")
            .text("(not enough data for graph)");
        return;
    }

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

function boxnwhisk(elem, data, thisKey, rev, lbl) {
    var data = data.filter(r => r.x != -1);
    var mult = rev ? -1 : 1;
    
    var sumstat = d3.nest() // nest function allows to group the calculation per level of a factor
        .key(function(d) { return d.y; })
        .rollup(function(d) {
            var q1 = d3.quantile(d.map(function(g) { return g.x; }).sort(d3.ascending), .25);
            var median = d3.quantile(d.map(function(g) { return g.x; }).sort(d3.ascending), .5);
            var q3 = d3.quantile(d.map(function(g) { return g.x; }).sort(d3.ascending), .75);
            var iqr = q3 - q1;
            var max = d3.max(d.map(function(g) { return g.x; }));
            var min = d3.min(d.map(function(g) { return g.x; }));
            return {
                q1: q1,
                median: median,
                q3: q3,
                iqr: iqr,
                min: Math.max(min, q1 - 1.5 * iqr),
                max: Math.min(max, q3 + 1.5 * iqr)
            };
        })
        .entries(data)
        .sort((a, b) => (a.value.median == b.value.median) ? 0 : (a.value.median < b.value.median) ? mult * 1 : mult * -1);

    const WIDTH        = 300;
    const BOX_HEIGHT   = 15;
    const BOX_INNER_HEIGHT = BOX_HEIGHT * 0.65;
    const AXIS_HEIGHT  = 30;
    const MARGIN       = { top: 4, right: 4, bottom: 4, left: 4 };
    const INNER_WIDTH  = WIDTH - MARGIN.left - MARGIN.right;

    const xmin = Math.min(...sumstat.map(r => r.value.min));
    const xmax = Math.max(...sumstat.map(r => r.value.max));

    const x   = d3.scaleLinear().domain([xmin, xmax]).range([0, INNER_WIDTH]);
    const y   = d3.scaleBand().domain(sumstat.map(function(d) { return d.key; })).paddingInner(1).paddingOuter(0.5);

    const HEIGHT       = BOX_HEIGHT * y.domain().length + AXIS_HEIGHT;
    const INNER_HEIGHT = HEIGHT - MARGIN.top - MARGIN.bottom - AXIS_HEIGHT;

    y.range([INNER_HEIGHT, 0]);

    const svg = d3.select(elem).append('svg')
          .attr('viewBox', `0 0 ${WIDTH} ${HEIGHT}`)
          .append('g')
          .attr('transform', 'translate(' + MARGIN.left + ',' + MARGIN.top + ')');

    svg
        .append("g")
        .attr("transform", `translate(0, ${INNER_HEIGHT + 5})`)
        .style("stroke-width", "0.35px")
        .style("font-size", "0.35em")
        .style("font-family", "Recursive Mono")
        .call(d3.axisBottom(x));

    svg.append("text")
        .attr("transform", `translate(${WIDTH / 2}, ${INNER_HEIGHT + 30})`)
        .style("stroke-width", "0.35px")
        .style("font-size", "0.40em")
        .style("font-family", "Recursive Mono")
        .style("text-anchor", "middle")
        .text(lbl);

    svg
        .selectAll("bgRects")
        .data(sumstat)
        .enter()
        .append("rect")
            .attr("x", 0)
            .attr("y", function(d) { return y(d.key) - BOX_HEIGHT / 2; })
            .attr("height", BOX_HEIGHT)
            .attr("width", WIDTH)
            .attr("class", function(d) { return (d.key === thisKey) ? "fill-current text-teal-100" : "fill-current text-white"; });

    svg
        .selectAll("texts")
        .data(sumstat)
        .enter()
        .append("text")
        .attr("x", function(d) { return (x(d.value.max) / INNER_WIDTH <= 0.85) ? x(d.value.max) + 6 : x(d.value.min) - 6; })
        .attr("y", function(d) { return y(d.key); })
        .style("font-size", "0.5em")
        .style("alignment-baseline", "middle")
        .style("text-anchor", function(d) { return (x(d.value.max) / INNER_WIDTH <= 0.85) ? "start" : "end"; })
        .attr("class", function(d) { return (d.key == thisKey) ? "fill-current text-teal-600" : "fill-current text-gray-500"; })
        .style("font-weight", function(d) { return (d.key == thisKey) ? "600" : "500"; })
        .style("white-space", "pre")
        .text(function(d) { return d.key.substr(0, d.key.indexOf(',')); });

    // Show the main vertical line
    svg
        .selectAll("horizLines")
        .data(sumstat)
        .enter()
        .append("line")
            .attr("x1", function(d) { return x(d.value.min); })
            .attr("x2", function(d) { return x(d.value.max); })
            .attr("y1", function(d) { return y(d.key); })
            .attr("y2", function(d) { return y(d.key); })
            .attr("stroke", "gray");

    // rectangle for the main box
    svg
        .selectAll("boxes")
        .data(sumstat)
        .enter()
        .append("rect")
            .attr("x", function(d) { return x(d.value.q1); })
            .attr("y", function(d) { return y(d.key) - BOX_INNER_HEIGHT / 2; })
            .attr("height", BOX_HEIGHT * 0.65)
            .attr("width", function(d) { return x(d.value.q3) - x(d.value.q1); })
            .attr("stroke", "gray")
        .attr("class", function(d) { return (d.key === thisKey) ? "fill-current text-teal-300" : "fill-current text-gray-300"; });

    svg
        .selectAll("minLines")
        .data(sumstat)
        .enter()
        .append("line")
            .attr("x1", function(d) { return x(d.value.min); })
            .attr("x2", function(d) { return x(d.value.min); })
            .attr("y1", function(d) { return y(d.key) - BOX_INNER_HEIGHT / 2; })
            .attr("y2", function(d) { return y(d.key) + BOX_INNER_HEIGHT / 2; })
            .attr("stroke", "gray");
    
    svg
        .selectAll("minLines")
        .data(sumstat)
        .enter()
        .append("line")
            .attr("x1", function(d) { return x(d.value.max); })
            .attr("x2", function(d) { return x(d.value.max); })
            .attr("y1", function(d) { return y(d.key) - BOX_INNER_HEIGHT / 2; })
            .attr("y2", function(d) { return y(d.key) + BOX_INNER_HEIGHT / 2; })
            .attr("stroke", "gray");
    svg
        .selectAll("medLines")
        .data(sumstat)
        .enter()
        .append("line")
            .attr("x1", function(d) { return x(d.value.median); })
            .attr("x2", function(d) { return x(d.value.median); })
            .attr("y1", function(d) { return y(d.key) - BOX_INNER_HEIGHT / 2; })
            .attr("y2", function(d) { return y(d.key) + BOX_INNER_HEIGHT / 2; })
            .attr("stroke", "gray");
}
