// margins and dimensions
const margin = { top: 20, right: 30, bottom: 70, left: 60 },
      width = 960 - margin.left - margin.right,
      height = 500 - margin.top - margin.bottom;

// SVG for scatter plot and bar
// and tooltip to display data on mouse hover
const scatterSvg = d3.select("#scatter-plot").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", `translate(${margin.left},${margin.top})`);
const barSvg = d3.select("#bar-chart").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", `translate(${margin.left},${margin.top})`);
const tooltip = d3.select("body").append("div")
  .attr("class", "tooltip")
  .style("opacity", 0);

// store variables
let selectedManufacturers = [];

// read data and do transformation
d3.csv("car.csv").then(function(data) {
  data.forEach(d => {
    d.Price = +d.Price;
    d.Year = +d.Year;
    d.Mileage = +d.Mileage;
  });

  
  // Define for scatterplot
  // color, size scale, coordinate axis, etc.
  const colorScale = d3.scaleOrdinal(d3.schemeCategory10);
  const sizeScale = d3.scaleLinear()
      .domain(d3.extent(data, d => d.Mileage))
      .range([3, 10]); 

  const xScale = d3.scaleLinear()
      .domain(d3.extent(data, d => d.Year))
      .range([0, width]);
  
  const yScale = d3.scaleLinear()
      .domain([0, d3.max(data, d => d.Price)])
      .range([height, 0]);

  scatterSvg.append("g")
      .attr("transform", `translate(0, ${height})`)
      .call(d3.axisBottom(xScale).tickFormat(d3.format("d")));

  scatterSvg.append("g")
      .call(d3.axisLeft(yScale));

  scatterSvg.selectAll(".dot")
      .data(data)
    .enter().append("circle")
      .attr("class", "dot")
      .attr("cx", d => xScale(d.Year))
      .attr("cy", d => yScale(d.Price))
      .attr("r", d => sizeScale(d.Mileage))
      .style("fill", d => colorScale(d.FuelType))
      .on("mouseover", (event, d) => {
        tooltip.transition().duration(200).style("opacity", 1);
        tooltip.html(`
          <strong>Model:</strong> ${d.Model}<br>
          <strong>Year:</strong> ${d.Year}<br>
          <strong>Price:</strong> $${d.Price}<br>
          <strong>Mileage:</strong> ${d.Mileage} km<br>
          <strong>Fuel Type:</strong> ${d.FuelType}
        `)
          .style("left", (event.pageX) + "px")
          .style("top", (event.pageY - 28) + "px");
      })
      .on("mouseout", () => {
        tooltip.transition().duration(500).style("opacity", 0);
      });

  // Group data by manufacturer
  const manufacturerCounts = Array.from(d3.group(data, d => d.Manufacturer), ([key, value]) => ({ Manufacturer: key, Count: value.length }));

  // Define for histogram
  // scale, coordinate axis
  // Add comparison 
  const xBarScale = d3.scaleBand()
      .domain(manufacturerCounts.map(d => d.Manufacturer))
      .range([0, width])
      .padding(0.1);

  const yBarScale = d3.scaleLinear()
      .domain([0, d3.max(manufacturerCounts, d => d.Count)])
      .range([height, 0]);


  barSvg.append("g")
      .attr("transform", `translate(0, ${height})`)
      .call(d3.axisBottom(xBarScale))
      .selectAll("text")
      .attr("y", 0)
      .attr("x", -9)
      .attr("dy", ".35em")
      .attr("transform", "rotate(-90)")
      .style("text-anchor", "end");

  barSvg.append("g")
      .call(d3.axisLeft(yBarScale));

  barSvg.selectAll(".bar")
    .data(manufacturerCounts)
  .enter().append("rect")
    .attr("class", "bar")
    .attr("x", d => xBarScale(d.Manufacturer))
    .attr("y", d => yBarScale(d.Count))
    .attr("width", xBarScale.bandwidth())
    .attr("height", d => height - yBarScale(d.Count))
    .on("click", function(event, d) {
        const manufacturer = d.Manufacturer;
        const index = selectedManufacturers.indexOf(manufacturer);
        if(index === -1) {
            selectedManufacturers.push(manufacturer);
            if(selectedManufacturers.length > 2) selectedManufacturers.shift();
        } else {
            selectedManufacturers.splice(index, 1);
        }
        barSvg.selectAll('.bar')
            .classed('selected', d => selectedManufacturers.includes(d.Manufacturer));
        updateInformationPanel(selectedManufacturers, data);
    })
    .on("mouseover", (event, d) => {
        tooltip.transition().duration(200).style("opacity", 1);
        tooltip.html(`Manufacturer: ${d.Manufacturer}<br>Count: ${d.Count}`)
            .style("left", (event.pageX) + "px")
            .style("top", (event.pageY - 28) + "px");
    })
    .on("mouseout", () => {
        tooltip.transition().duration(500).style("opacity", 0);
    });
});

// Calculate manufacturer's average price
// Update panel information
// Clear previous content
function updateInformationPanel(selectedManufacturers, data) {
    const infoPanel = d3.select("#info-panel");
    infoPanel.html(""); 

    selectedManufacturers.forEach(manufacturer => {
        const manufacturerData = data.filter(d => d.Manufacturer === manufacturer);
        const averagePrice = d3.mean(manufacturerData, d => d.Price).toFixed(2);
        infoPanel.append("div")
            .html(`Manufacturer: ${manufacturer}<br>Average Price: $${averagePrice}`);
    });
}
