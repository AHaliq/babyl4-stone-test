// function Tree(data, { // data is either tabular (array of objects) or hierarchy (nested objects)
//   path, // as an alternative to id and parentId, returns an array identifier, imputing internal nodes
//   id = Array.isArray(data) ? d => d.id : null, // if tabular data, given a d in data, returns a unique identifier (string)
//   parentId = Array.isArray(data) ? d => d.parentId : null, // if tabular data, given a node d, returns its parent’s identifier
//   children, // if hierarchical data, given a d in data, returns its children
//   tree = d3.tree, // layout algorithm (typically d3.tree or d3.cluster)
//   sort, // how to sort nodes prior to layout (e.g., (a, b) => d3.descending(a.height, b.height))
//   label, // given a node d, returns the display name
//   title, // given a node d, returns its hover text
//   link, // given a node d, its link (if any)
//   linkTarget = "_blank", // the target attribute for links (if any)
//   width, // outer width, in pixels
//   widthUnit,
//   height, // outer height, in pixels
//   r = 3, // radius of nodes
//   padding = 1, // horizontal padding for first and last column
//   fill = themes.light.foreground, // fill for nodes
//   fillOpacity, // fill opacity for nodes
//   stroke = themes.light.selectionBackground, // stroke for links
//   strokeWidth = 1.5, // stroke width for links
//   strokeOpacity = 0.4, // stroke opacity for links
//   strokeLinejoin, // stroke line join for links
//   strokeLinecap, // stroke line cap for links
//   halo = themes.light.background, // color of label halo 
//   haloWidth = 3, // padding around the labels
// } = {}) {

//   // If a path accessor is specified, we can impute the internal nodes from the slash-
//   // separated path; otherwise, the tabular data must include the internal nodes, not
//   // just leaves. TODO https://github.com/d3/d3-hierarchy/issues/33
//   if (path != null) {
//     const D = d3.map(data, d => d);
//     const I = d3.map(data, path).map(d => (d = `${d}`).startsWith("/") ? d : `/${d}`);
//     const paths = new Set(I);
//     for (const path of paths) {
//       const parts = path.split("/");
//       while (parts.pop(), parts.length) {
//         const path = parts.join("/") || "/";
//         if (paths.has(path)) continue;
//         paths.add(path), I.push(path), D.push(null);
//       }
//     }
//     id = (_, i) => I[i];
//     parentId = (_, i) => I[i] === "/" ? "" : I[i].slice(0, I[i].lastIndexOf("/")) || "/";
//     data = D;
//   }

//   // If id and parentId options are specified (perhaps implicitly via the path option),
//   // use d3.stratify to convert tabular data to a hierarchy; otherwise we assume that
//   // the data is specified as an object {children} with nested objects (a.k.a. the
//   // “flare.json” format), and use d3.hierarchy.
//   const root = id == null && parentId == null
//     ? d3.hierarchy(data, children)
//     : d3.stratify().id(id).parentId(parentId)(data);

//   function maxDepth(n, m = 0) {
//     let mm = n.depth > m ? n.depth : m;
//     return (n.children ?? []).map(c => maxDepth(c, mm)).reduce((a, x) => Math.max(a, x), mm);
//   }
//   if (widthUnit) width = widthUnit * maxDepth(root);
//   // Compute labels and titles.
//   const descendants = root.descendants();
//   const L = label == null ? null : descendants.map(d => label(d.data, d));

//   // Sort the nodes.
//   if (sort != null) root.sort(sort);

//   // Compute the layout.
//   const dx = 10;
//   const dy = width / (root.height + padding);
//   tree().nodeSize([dx, dy])(root);

//   // Center the tree.
//   let x0 = Infinity;
//   let x1 = -x0;
//   root.each(d => {
//     if (d.x > x1) x1 = d.x;
//     if (d.x < x0) x0 = d.x;
//   });

//   // Compute the default height.
//   if (height === undefined) height = x1 - x0 + dx * 2;

//   const svg = d3.create("svg")
//     .attr("viewBox", [-dy * padding / 2, x0 - dx, width, height])
//     .attr("width", width)
//     .attr("height", height)
//     .attr("style", "height: auto; height: intrinsic;")
//     .attr("font-family", "sans-serif")
//     .attr("font-size", 10);

//   svg.append("g")
//     .attr("fill", "none")
//     .attr("stroke", stroke)
//     .attr("stroke-opacity", strokeOpacity)
//     .attr("stroke-linecap", strokeLinecap)
//     .attr("stroke-linejoin", strokeLinejoin)
//     .attr("stroke-width", strokeWidth)
//     .selectAll("path")
//     .data(root.links())
//     .join("path")
//     .attr("d", d3.linkHorizontal()
//       .x(d => d.y)
//       .y(d => d.x));

//   const node = svg.append("g")
//     .selectAll("a")
//     .data(root.descendants())
//     .join("a")
//     .attr("xlink:href", link == null ? null : d => link(d.data, d))
//     .attr("target", link == null ? null : linkTarget)
//     .attr("transform", d => `translate(${d.y},${d.x})`);

//   node.append("circle")
//     .attr("fill", d => d.children ? stroke : fill)
//     .attr("r", r);

//   if (title != null) node.append("title")
//     .text(d => title(d.data, d));

//   if (L) node.append("text")
//     .attr("fill", themes.light.foreground)
//     .attr("dy", "0.32em")
//     .attr("x", d => d.children ? -6 : 6)
//     .attr("text-anchor", d => d.children ? "end" : "start")
//     .text((d, i) => L[i])
//     .call(text => text.clone(true))
//     .attr("fill", "none")
//     .attr("stroke", halo)
//     .attr("stroke-width", haloWidth);

//   return svg.node();
// }

// function radial_tree(editor, dataobj) {
//   const width = 954;
//   const radius = width / 2;
//   const tree = d3.tree()
//     .size([2 * Math.PI, radius])
//     .separation((a, b) => (a.parent == b.parent ? 1 : 2) / a.depth);
//   const data = d3.hierarchy(dataobj)
//     .sort((a, b) => d3.ascending(a.data.name, b.data.name));

//   const root = tree(data);

//   const svg = d3.create('svg');

//   svg.append('g')
//     .attr('fill', 'none')
//     .attr('stroke', themes.light.selectionBackground)
//     .attr('stroke-opacity', 0.4)
//     .attr('stroke-width', 1.5)
//     .selectAll('path')
//     .data(root.links())
//     .join('path')
//     .attr('d', d3.linkRadial()
//       .angle(d => d.x)
//       .radius(d => d.y));

//   svg.append('g')
//     .selectAll('circle')
//     .data(root.descendants())
//     .join('circle')
//     .attr('transform', d => `
//                   rotate(${d.x * 180 / Math.PI - 90})
//                   translate(${d.y},0)
//                 `)
//     .attr('fill', d => d.children ? themes.light.selectionBackground : themes.light.foreground)
//     .attr('r', 2.5);

//   /*
// svg.append('g')
//   .attr('font-family', 'sans-serif')
//   .attr('font-size', 10)
//   .attr('fill', themes.light.foreground)
//   .attr('stroke-linejoin', 'round')
//   .attr('stroke-width', 3)
//   .selectAll('text')
//   .data(root.descendants())
//   .join('text')
//   .attr('transform', d => `
//                 rotate(${d.x * 180 / Math.PI - 90}) 
//                 translate(${d.y},0) 
//                 rotate(${d.x >= Math.PI ? 180 : 0})
//               `)
//   .attr('dy', '0.31em')
//   .attr('x', d => d.x < Math.PI === !d.children ? 6 : -6)
//   .attr('text-anchor', d => d.x < Math.PI === !d.children ? 'start' : 'end')
//   .text(d => d.data.name)
//   .clone(true).lower()
//   .attr('stroke', themes.light.background);*/

//   function autobox() {
//     document.body.appendChild(this);
//     const { x, y, width, height } = this.getBBox();
//     document.body.removeChild(this);
//     return [x, y, width, height];
//   }
//   const c = svg.attr('viewBox', autobox).node();
//   return c;
// }

// function indent_tree(editor, dataobj) {
//   // Copyright 2021 Observable, Inc.
//   //   Released under the ISC license.
//   //   https://observablehq.com/@d3/tree
//   const format = d3.format(",");
//   let i = 0;
//   let root = d3.hierarchy(dataobj).eachBefore(d => d.index = i++);
//   let nodeSize = 17;
//   const width = 600;
//   const columns = [
//     {
//       label: "start:line",
//       value: d => d?.SRng?.start?.line ?? "-",
//       format,
//       x: 280
//     },
//     {
//       label: "start:col",
//       value: d => d?.SRng?.start?.col ?? "-",
//       format,
//       x: 340
//     },
//     {
//       label: "end:line",
//       value: d => d?.SRng?.end?.line ?? "-",
//       format,
//       x: 400
//     },
//     {
//       label: "end:col",
//       value: d => d?.SRng?.end?.col ?? "-",
//       format,
//       x: 460
//     },
//     {
//       label: "Count",
//       value: d => d.children ? 0 : 1,
//       format: (value, d) => d.children ? format(value) : "-",
//       x: 520
//     }
//   ]
//   const nodes = root.descendants();

//   const svg = d3.create("svg")
//     .attr("viewBox", [-nodeSize / 2, -nodeSize * 3 / 2, width, (nodes.length + 1) * nodeSize])
//     .attr("font-family", "sans-serif")
//     .attr("font-size", 10)
//     .style("overflow", "visible");

//   const link = svg.append("g")
//     .attr("fill", "none")
//     .attr("stroke", themes.light.foreground)
//     .selectAll("path")
//     .data(root.links())
//     .join("path")
//     .attr("d", d => `
//         M${d.source.depth * nodeSize},${d.source.index * nodeSize}
//         V${d.target.index * nodeSize}
//         h${nodeSize}
//       `);

//   const node = svg.append("g")
//     .selectAll("g")
//     .data(nodes)
//     .join("g")
//     .attr("transform", d => `translate(0,${d.index * nodeSize})`);

//   node.append("circle")
//     .attr("cx", d => d.depth * nodeSize)
//     .attr("r", 2.5)
//     .attr("fill", d => d.children ? themes.light.selectionBackground : themes.light.foreground);

//   node.append("text")
//     .attr("fill", d => d.children ? themes.light.selectionBackground : themes.light.foreground)
//     .attr("dy", "0.32em")
//     .attr("x", d => d.depth * nodeSize + 6)
//     .text(d => d.data.name);

//   node.append("title")
//     .text(d => d.ancestors().reverse().map(d => d.data.name).join("/"));

//   for (const { label, value, format, x } of columns) {
//     svg.append("text")
//       .attr("fill", themes.light.foreground)
//       .attr("dy", "0.32em")
//       .attr("y", -nodeSize)
//       .attr("x", x)
//       .attr("text-anchor", "end")
//       .attr("font-weight", "bold")
//       .text(label);

//     node.append("text")
//       .attr("dy", "0.32em")
//       .attr("x", x)
//       .attr("text-anchor", "end")
//       .attr("fill", d => d.children ? themes.light.selectionBackground : themes.light.foreground)
//       .data(root.copy().sum(value).descendants())
//       .text(d => format(d.value, d));
//   }
//   return svg.node();
// }

function force_tree(editor, dataobj, {
  parents = [],
  svg = null,
  wdt = 200,
  hgt = 200,
  domele = null
} = {}) {
  drag = simulation => {

    function dragstarted(event, d) {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      d.fx = d.x;
      d.fy = d.y;
    }

    function dragged(event, d) {
      d.fx = event.x;
      d.fy = event.y;
    }

    function dragended(event, d) {
      if (!event.active) simulation.alphaTarget(0);
      d.fx = null;
      d.fy = null;
    }

    return d3.drag()
      .on("start", dragstarted)
      .on("drag", dragged)
      .on("end", dragended);
  }

  const maxNodes = 15;
  const minDepth = 2;
  const prune = d => {
    let co = 0;
    let result = { children: [] };
    let q = [{ d: d, ps: parents, cd: 0, p: result }];
    while (q.length > 0) {
      co++;
      const { d, ps, cd, p } = q.shift();
      let x = {
        "name": d.name,
        "SRng": d.SRng,
        "more": false,
        "orig": d
      };
      if (ps.length > 0) x.parents = [...ps];
      if (d.children) {
        d.children.forEach(d2 => {
          if (cd < minDepth || co < maxNodes) {
            if (!x.children) x.children = [];
            q.push({ d: d2, ps: [...ps, d], cd: cd + 1, p: x });
          } else {
            x.more = true;
          }
        });
      }
      p.children.push(x);
    }
    return result.children[0];
  }

  const prunewrap = (d) => {
    let data = prune(d);
    if (parents.length > 0) {
      p2 = [...parents];
      const ori = p2.pop();
      data = {
        "name": "parent",
        "SRng": null,
        "more": false,
        "orig": ori,
        "parents": p2,
        "children": [data]
      }
      data.toplevel = true;
    }
    const root = d3.hierarchy(data);
    return { data: data, root: root, links: root.links(), nodes: root.descendants() };
  }

  const getDepth = (d, v = 1) => {
    return d.children ? d.children.map(x => getDepth(x, v + 1)).reduce((a, x) => Math.max(a, x), v) : v;
  }

  svg = svg ?? d3.create("svg");
  const maximizeRes = () => {
    const width = domele.clientWidth;
    const height = domele.clientHeight;
    svg.node().setAttribute("viewBox", `${Math.round(-width / 2)} ${Math.round(-height / 2)} ${width} ${height}`);
    svg.node().setAttribute("width", `${width}`);
    svg.node().setAttribute("height", `${height}`);
  };
  maximizeRes();
  window.addEventListener("resize", maximizeRes);

  const { data, root, links, nodes } = prunewrap(dataobj);

  const curDepth = getDepth(data);

  const scale = 5;
  const simulation = d3.forceSimulation(nodes)
    .force("link", d3.forceLink(links).id(d => d.id).distance(0).strength(1))
    .force("charge", d3.forceManyBody().strength(-50 * scale))
    .force("x", d3.forceX())
    .force("y", d3.forceY());


  const link = svg.append("g")
    .attr("stroke", "#999")
    .attr("stroke-opacity", 0.6)
    .selectAll("line")
    .data(links)
    .join("line");

  const texts = svg.append("g")
    .selectAll(".texts")
    .data(nodes)
    .join("text")
    .attr("style", "user-select: none; font-family: 'Roboto Mono', monospace; font-size: 1rem;")
    .attr("fill", d => d.data.children || d.data.more ? themes.light.selectionBackground : themes.light.foreground)
    .attr("dy", "0.32em")
    .attr("dx", 12)
    .text(d => d.data.more || d.data.toplevel ? "" : d.data.name);

  const nscale = 2;
  const node = svg.append("g")
    .attr("fill", themes.light.white)
    .attr("stroke", themes.light.black)
    .attr("stroke-width", 1.5 * nscale)
    .selectAll("circle")
    .data(nodes)
    .join("circle")
    .attr("fill", d => d.data.children ? (d.data.toplevel ? themes.light.selectionBackground : themes.light.white) : themes.light.black)
    .attr("stroke", d => d.data.children ? (d.data.toplevel ? themes.light.selectionBackground : themes.light.black) : themes.light.brightBlack)
    .attr("stroke-dasharray", d => d.data.more ? `${nscale},${nscale}` : null)
    .attr("r", 3.5 * nscale)
    .on("click", (d, i) => {
      if (i.data.toplevel || i.data.more) {
        svg.selectAll("g").remove();
        force_tree(editor, i.data.orig, { svg: svg, parents: i.data.parents });
        // more node and parent node
      } else {
        let range = new ace.Range(
          parseInt(i.data.SRng.start.line) - 1,
          parseInt(i.data.SRng.start.col) - 1,
          parseInt(i.data.SRng.end.line) - 1,
          parseInt(i.data.SRng.end.col) - 1
        );
        editor.selection.clearSelection();
        editor.selection.setRange(range);
      }
    })
    .call(drag(simulation));

  node.append("title")
    .text(d => d.data.name);

  simulation.on("tick", () => {
    link
      .attr("x1", d => d.source.x)
      .attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x)
      .attr("y2", d => d.target.y);

    node
      .attr("cx", d => d.x)
      .attr("cy", d => d.y);

    texts
      .attr("x", d => d.x)
      .attr("y", d => d.y);
  });
  return svg.node();
}

// function tree(editor, dataobj) {
//   const c = Tree(dataobj, {
//     label: d => d.name,
//     //sort: (a, b) => d3.descending(a.height, b.height), // reduce link crossings
//     tree: d3.cluster,
//     widthUnit: 140
//   });
//   return c;
// }

// HASKELL ASTTAB to JS INTERFACE ------------

/**
 * Generates svg elements and puts them as child of the specified dom element
 */
function d3render(domele, editor, datajson) {
  f = force_tree;
  if (datajson === "") {
    if (domele)
      while (domele.firstChild) domele.removeChild(domele.firstChild);
    return;
  }
  const dataparse = JSON.parse(datajson);
  console.log(dataparse);
  const c = f(editor, dataparse, { wdt: domele.clientWidth, hgt: domele.clientHeight, domele: domele });
  if (domele) {
    while (domele.firstChild) domele.removeChild(domele.firstChild);
    domele.appendChild(c);
  }
  return c;
}

/**
 * Observer that watches for all children nodes being added to body and caches them
 */
let lastRenderAction = null;
let domCache = {};
(() => {
  const targetNode = document.body;
  const config = { attributes: true, childList: true, subtree: true };
  const observer = new MutationObserver((mutationList, observer) => {
    mutationList.filter(m => m.type === 'childList' && m.addedNodes.length > 0).forEach(m => {
      m.addedNodes.forEach(ele => {
        domCache[ele.className] = ele;
        if (lastRenderAction !== null && ele.className === lastRenderAction.a) {
          d3render(ele, lastRenderAction.b, lastRenderAction.c);
        }
      });
    });
  });
  observer.observe(targetNode, config);
})();

/**
 * Informs Observer of a render action
 * if domelement is already in the dom; render d3 immediately
 * if domelement is not in the dom wait till observer catches it; render d3 immediately
 * if both happens, the latter will ensure valid dom element instead of old removed d3 container
 */
function d3renderOnEleAddToNode(domclass, editor, datajson) {
  lastRenderAction = { a: domclass, b: editor, c: datajson };
  const domele = domCache[domclass];
  if (domele) {
    d3render(domele, editor, datajson);
  }
}