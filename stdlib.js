// @observablehq/stdlib v3.15.0 Copyright 2021 Observable, Inc.
!function(e,t){"object"==typeof exports&&"undefined"!=typeof module?t(exports):"function"==typeof define&&define.amd?define(["exports"],t):t((e="undefined"!=typeof globalThis?globalThis:e||self).observablehq=e.observablehq||{})}(this,(function(e){"use strict";var t={},n={};function r(e){return new Function("d","return {"+e.map((function(e,t){return JSON.stringify(e)+": d["+t+'] || ""'})).join(",")+"}")}function o(e){var t=Object.create(null),n=[];return e.forEach((function(e){for(var r in e)r in t||n.push(t[r]=r)})),n}function i(e,t){var n=e+"",r=n.length;return r<t?new Array(t-r+1).join(0)+n:n}function a(e){var t,n=e.getUTCHours(),r=e.getUTCMinutes(),o=e.getUTCSeconds(),a=e.getUTCMilliseconds();return isNaN(e)?"Invalid Date":((t=e.getUTCFullYear())<0?"-"+i(-t,6):t>9999?"+"+i(t,6):i(t,4))+"-"+i(e.getUTCMonth()+1,2)+"-"+i(e.getUTCDate(),2)+(a?"T"+i(n,2)+":"+i(r,2)+":"+i(o,2)+"."+i(a,3)+"Z":o?"T"+i(n,2)+":"+i(r,2)+":"+i(o,2)+"Z":r||n?"T"+i(n,2)+":"+i(r,2)+"Z":"")}function s(e){var i=new RegExp('["'+e+"\n\r]"),s=e.charCodeAt(0);function u(e,r){var o,i=[],a=e.length,u=0,l=0,c=a<=0,f=!1;function d(){if(c)return n;if(f)return f=!1,t;var r,o,i=u;if(34===e.charCodeAt(i)){for(;u++<a&&34!==e.charCodeAt(u)||34===e.charCodeAt(++u););return(r=u)>=a?c=!0:10===(o=e.charCodeAt(u++))?f=!0:13===o&&(f=!0,10===e.charCodeAt(u)&&++u),e.slice(i+1,r-1).replace(/""/g,'"')}for(;u<a;){if(10===(o=e.charCodeAt(r=u++)))f=!0;else if(13===o)f=!0,10===e.charCodeAt(u)&&++u;else if(o!==s)continue;return e.slice(i,r)}return c=!0,e.slice(i,a)}for(10===e.charCodeAt(a-1)&&--a,13===e.charCodeAt(a-1)&&--a;(o=d())!==n;){for(var h=[];o!==t&&o!==n;)h.push(o),o=d();r&&null==(h=r(h,l++))||i.push(h)}return i}function l(t,n){return t.map((function(t){return n.map((function(e){return f(t[e])})).join(e)}))}function c(t){return t.map(f).join(e)}function f(e){return null==e?"":e instanceof Date?a(e):i.test(e+="")?'"'+e.replace(/"/g,'""')+'"':e}return{parse:function(e,t){var n,o,i=u(e,(function(e,i){if(n)return n(e,i-1);o=e,n=t?function(e,t){var n=r(e);return function(r,o){return t(n(r),o,e)}}(e,t):r(e)}));return i.columns=o||[],i},parseRows:u,format:function(t,n){return null==n&&(n=o(t)),[n.map(f).join(e)].concat(l(t,n)).join("\n")},formatBody:function(e,t){return null==t&&(t=o(e)),l(e,t).join("\n")},formatRows:function(e){return e.map(c).join("\n")},formatRow:c,formatValue:f}}var u=s(","),l=u.parse,c=u.parseRows,f=s("\t"),d=f.parse,h=f.parseRows;function m(e){for(var t in e){var n,r,o=e[t].trim();if(o)if("true"===o)o=!0;else if("false"===o)o=!1;else if("NaN"===o)o=NaN;else if(isNaN(n=+o)){if(!(r=o.match(/^([-+]\d{2})?\d{4}(-\d{2}(-\d{2})?)?(T\d{2}:\d{2}(:\d{2}(\.\d{3})?)?(Z|[-+]\d{2}:\d{2})?)?$/)))continue;p&&r[4]&&!r[7]&&(o=o.replace(/-/g,"/").replace(/T/," ")),o=new Date(o)}else o=n;else o=null;e[t]=o}return e}const p=new Date("2019-01-01T00:00").getHours()||new Date("2019-07-01T00:00").getHours(),w=new Map,v=[],y=v.map,g=v.some,b=v.hasOwnProperty,x="https://cdn.jsdelivr.net/npm/",j=/^((?:@[^/@]+\/)?[^/@]+)(?:@([^/]+))?(?:\/(.*))?$/,E=/^\d+\.\d+\.\d+(-[\w-.+]+)?$/,A=/\.[^/]*$/,P=["unpkg","jsdelivr","browser","main"];class RequireError extends Error{constructor(e){super(e)}}function C(e){const t=j.exec(e);return t&&{name:t[1],version:t[2],path:t[3]}}function O(e){const t=`${x}${e.name}${e.version?`@${e.version}`:""}/package.json`;let n=w.get(t);return n||w.set(t,n=fetch(t).then((e=>{if(!e.ok)throw new RequireError("unable to load package.json");return e.redirected&&!w.has(e.url)&&w.set(e.url,n),e.json()}))),n}RequireError.prototype.name=RequireError.name;var N=$((async function(e,t){if(e.startsWith(x)&&(e=e.substring(x.length)),/^(\w+:)|\/\//i.test(e))return e;if(/^[.]{0,2}\//i.test(e))return new URL(e,null==t?location:t).href;if(!e.length||/^[\s._]/.test(e)||/\s$/.test(e))throw new RequireError("illegal name");const n=C(e);if(!n)return`${x}${e}`;if(!n.version&&null!=t&&t.startsWith(x)){const e=await O(C(t.substring(x.length)));n.version=e.dependencies&&e.dependencies[n.name]||e.peerDependencies&&e.peerDependencies[n.name]}if(n.path&&!A.test(n.path)&&(n.path+=".js"),n.path&&n.version&&E.test(n.version))return`${x}${n.name}@${n.version}/${n.path}`;const r=await O(n);return`${x}${r.name}@${r.version}/${n.path||function(e){for(const t of P){const n=e[t];if("string"==typeof n)return A.test(n)?n:`${n}.js`}}(r)||"index.js"}`}));function $(e){const t=new Map,n=o(null);function r(e){if("string"!=typeof e)return e;let n=t.get(e);return n||t.set(e,n=new Promise(((t,n)=>{const r=document.createElement("script");r.onload=()=>{try{t(v.pop()(o(e)))}catch(e){n(new RequireError("invalid module"))}r.remove()},r.onerror=()=>{n(new RequireError("unable to load module")),r.remove()},r.async=!0,r.src=e,window.define=T,document.head.appendChild(r)}))),n}function o(t){return n=>Promise.resolve(e(n,t)).then(r)}function i(e){return arguments.length>1?Promise.all(y.call(arguments,n)).then(k):n(e)}return i.alias=function(t){return $(((n,r)=>n in t&&(r=null,"string"!=typeof(n=t[n]))?n:e(n,r)))},i.resolve=e,i}function k(e){const t={};for(const n of e)for(const e in n)b.call(n,e)&&(null==n[e]?Object.defineProperty(t,e,{get:L(n,e)}):t[e]=n[e]);return t}function L(e,t){return()=>e[t]}function R(e){return"exports"===(e+="")||"module"===e}function T(e,t,n){const r=arguments.length;r<2?(n=e,t=[]):r<3&&(n=t,t="string"==typeof e?[]:e),v.push(g.call(t,R)?e=>{const r={},o={exports:r};return Promise.all(y.call(t,(t=>"exports"===(t+="")?r:"module"===t?o:e(t)))).then((e=>(n.apply(null,e),o.exports)))}:e=>Promise.all(y.call(t,e)).then((e=>"function"==typeof n?n.apply(null,e):n)))}function q(e,t,n){return{resolve:(r=n)=>`https://cdn.jsdelivr.net/npm/${e}@${t}/${r}`}}T.amd={};const M=q("d3","7.1.1","dist/d3.min.js"),U=q("@observablehq/inputs","0.10.3","dist/inputs.min.js"),S=q("@observablehq/plot","0.2.9","dist/plot.umd.min.js"),_=q("@observablehq/graphviz","0.2.1","dist/graphviz.min.js"),D=q("@observablehq/highlight.js","2.0.0","highlight.min.js"),F=q("@observablehq/katex","0.11.1","dist/katex.min.js"),B=q("lodash","4.17.21","lodash.min.js"),z=q("htl","0.3.1","dist/htl.min.js"),W=q("jszip","3.7.1","dist/jszip.min.js"),H=q("marked","0.3.12","marked.min.js"),Z=q("sql.js","1.6.2","dist/sql-wasm.js"),I=q("vega","5.21.0","build/vega.min.js"),V=q("vega-lite","5.1.1","build/vega-lite.min.js"),Q=q("vega-lite-api","5.0.0","build/vega-lite-api.min.js"),J=q("apache-arrow","4.0.1","Arrow.es2015.min.js"),X=q("arquero","4.8.7","dist/arquero.min.js"),Y=q("topojson-client","3.1.0","dist/topojson-client.min.js"),G=q("exceljs","4.3.0","dist/exceljs.min.js");async function K(e){return(await e(Z.resolve()))({locateFile:e=>Z.resolve(`dist/${e}`)})}class SQLiteDatabaseClient{constructor(e){Object.defineProperties(this,{_db:{value:e}})}static async open(e){const[t,n]=await Promise.all([K(N),Promise.resolve(e).then(ee)]);return new SQLiteDatabaseClient(new t.Database(n))}async query(e,t){return await async function(e,t,n){const[r]=await e.exec(t,n);if(!r)return[];const{columns:o,values:i}=r,a=i.map((e=>Object.fromEntries(e.map(((e,t)=>[o[t],e])))));return a.columns=o,a}(this._db,e,t)}async queryRow(e,t){return(await this.query(e,t))[0]||null}async explain(e,t){return te("pre",{className:"observablehq--inspect"},[ne((await this.query(`EXPLAIN QUERY PLAN ${e}`,t)).map((e=>e.detail)).join("\n"))])}async describe(e){const t=await(void 0===e?this.query("SELECT name FROM sqlite_master WHERE type = 'table'"):this.query("SELECT * FROM pragma_table_info(?)",[e]));if(!t.length)throw new Error("Not found");const{columns:n}=t;return te("table",{value:t},[te("thead",[te("tr",n.map((e=>te("th",[ne(e)]))))]),te("tbody",t.map((e=>te("tr",n.map((t=>te("td",[ne(e[t])])))))))])}async sql(e,...t){return this.query(e.join("?"),t)}}function ee(e){return"string"==typeof e?fetch(e).then(ee):e instanceof Response||e instanceof Blob?e.arrayBuffer().then(ee):e instanceof ArrayBuffer?new Uint8Array(e):e}function te(e,t,n){2===arguments.length&&(n=t,t=void 0);const r=document.createElement(e);if(void 0!==t)for(const e in t)r[e]=t[e];if(void 0!==n)for(const e of n)r.appendChild(e);return r}function ne(e){return document.createTextNode(e)}Object.defineProperty(SQLiteDatabaseClient.prototype,"dialect",{value:"sqlite"});class Workbook{constructor(e){Object.defineProperties(this,{_:{value:e},sheetNames:{value:e.worksheets.map((e=>e.name)),enumerable:!0}})}sheet(e,t){const n="number"==typeof e?this.sheetNames[e]:this.sheetNames.includes(e+="")?e:null;if(null==n)throw new Error(`Sheet not found: ${e}`);return function(e,{range:t,headers:n}={}){let[[r,o],[i,a]]=function(e=":",{columnCount:t,rowCount:n}){if(!(e+="").match(/^[A-Z]*\d*:[A-Z]*\d*$/))throw new Error("Malformed range specifier");const[[r=0,o=0],[i=t-1,a=n-1]]=e.split(":").map(ae);return[[r,o],[i,a]]}(t,e);const s=n?e._rows[o++]:null;let u=new Set(["#"]);for(let e=r;e<=i;e++){const t=s?re(s.findCell(e+1)):null;let n=t&&t+""||ie(e);for(;u.has(n);)n+="_";u.add(n)}u=new Array(r).concat(Array.from(u));const l=new Array(a-o+1);for(let t=o;t<=a;t++){const n=l[t-o]=Object.create(null,{"#":{value:t+1}}),a=e.getRow(t+1);if(a.hasValues)for(let e=r;e<=i;e++){const t=re(a.findCell(e+1));null!=t&&(n[u[e+1]]=t)}}return l.columns=u.filter((()=>!0)),l}(this._.getWorksheet(n),t)}}function re(e){if(!e)return;const{value:t}=e;if(t&&"object"==typeof t&&!(t instanceof Date)){if(t.formula||t.sharedFormula)return t.result&&t.result.error?NaN:t.result;if(t.richText)return oe(t);if(t.text){let{text:e}=t;return e.richText&&(e=oe(e)),t.hyperlink&&t.hyperlink!==e?`${t.hyperlink} ${e}`:e}return t}return t}function oe(e){return e.richText.map((e=>e.text)).join("")}function ie(e){let t="";e++;do{t=String.fromCharCode(64+(e%26||26))+t}while(e=Math.floor((e-1)/26));return t}function ae(e){const[,t,n]=e.match(/^([A-Z]*)(\d*)$/);let r=0;if(t)for(let e=0;e<t.length;e++)r+=Math.pow(26,t.length-e-1)*(t.charCodeAt(e)-64);return[r?r-1:void 0,n?+n-1:void 0]}async function se(e){const t=await fetch(await e.url());if(!t.ok)throw new Error(`Unable to load file: ${e.name}`);return t}async function ue(e,t,{array:n=!1,typed:r=!1}={}){const o=await e.text();return("\t"===t?n?h:d:n?c:l)(o,r&&m)}class le{constructor(e){Object.defineProperty(this,"name",{value:e,enumerable:!0})}async blob(){return(await se(this)).blob()}async arrayBuffer(){return(await se(this)).arrayBuffer()}async text(){return(await se(this)).text()}async json(){return(await se(this)).json()}async stream(){return(await se(this)).body}async csv(e){return ue(this,",",e)}async tsv(e){return ue(this,"\t",e)}async image(e){const t=await this.url();return new Promise(((n,r)=>{const o=new Image;new URL(t,document.baseURI).origin!==new URL(location).origin&&(o.crossOrigin="anonymous"),Object.assign(o,e),o.onload=()=>n(o),o.onerror=()=>r(new Error(`Unable to load file: ${this.name}`)),o.src=t}))}async arrow(){const[e,t]=await Promise.all([N(J.resolve()),se(this)]);return e.Table.from(t)}async sqlite(){return SQLiteDatabaseClient.open(se(this))}async zip(){const[e,t]=await Promise.all([N(W.resolve()),this.arrayBuffer()]);return new ZipArchive(await e.loadAsync(t))}async xml(e="application/xml"){return(new DOMParser).parseFromString(await this.text(),e)}async html(){return this.xml("text/html")}async xlsx(){const[e,t]=await Promise.all([N(G.resolve()),this.arrayBuffer()]);return new Workbook(await(new e.Workbook).xlsx.load(t))}}class FileAttachment extends le{constructor(e,t){super(t),Object.defineProperty(this,"_url",{value:e})}async url(){return await this._url+""}}function ce(e){throw new Error(`File not found: ${e}`)}class ZipArchive{constructor(e){Object.defineProperty(this,"_",{value:e}),this.filenames=Object.keys(e.files).filter((t=>!e.files[t].dir))}file(e){const t=this._.file(e+="");if(!t||t.dir)throw new Error(`file not found: ${e}`);return new ZipArchiveEntry(t)}}class ZipArchiveEntry extends le{constructor(e){super(e.name),Object.defineProperty(this,"_",{value:e}),Object.defineProperty(this,"_url",{writable:!0})}async url(){return this._url||(this._url=this.blob().then(URL.createObjectURL))}async blob(){return this._.async("blob")}async arrayBuffer(){return this._.async("arraybuffer")}async text(){return this._.async("text")}async json(){return JSON.parse(await this.text())}}var fe={math:"http://www.w3.org/1998/Math/MathML",svg:"http://www.w3.org/2000/svg",xhtml:"http://www.w3.org/1999/xhtml",xlink:"http://www.w3.org/1999/xlink",xml:"http://www.w3.org/XML/1998/namespace",xmlns:"http://www.w3.org/2000/xmlns/"};var de=0;function he(e){this.id=e,this.href=new URL(`#${e}`,location)+""}he.prototype.toString=function(){return"url("+this.href+")"};var me={canvas:function(e,t){var n=document.createElement("canvas");return n.width=e,n.height=t,n},context2d:function(e,t,n){null==n&&(n=devicePixelRatio);var r=document.createElement("canvas");r.width=e*n,r.height=t*n,r.style.width=e+"px";var o=r.getContext("2d");return o.scale(n,n),o},download:function(e,t="untitled",n="Save"){const r=document.createElement("a"),o=r.appendChild(document.createElement("button"));async function i(){await new Promise(requestAnimationFrame),URL.revokeObjectURL(r.href),r.removeAttribute("href"),o.textContent=n,o.disabled=!1}return o.textContent=n,r.download=t,r.onclick=async t=>{if(o.disabled=!0,r.href)return i();o.textContent="Saving…";try{const t=await("function"==typeof e?e():e);o.textContent="Download",r.href=URL.createObjectURL(t)}catch(e){o.textContent=n}if(t.eventPhase)return i();o.disabled=!1},r},element:function(e,t){var n,r=e+="",o=r.indexOf(":");o>=0&&"xmlns"!==(r=e.slice(0,o))&&(e=e.slice(o+1));var i=fe.hasOwnProperty(r)?document.createElementNS(fe[r],e):document.createElement(e);if(t)for(var a in t)o=(r=a).indexOf(":"),n=t[a],o>=0&&"xmlns"!==(r=a.slice(0,o))&&(a=a.slice(o+1)),fe.hasOwnProperty(r)?i.setAttributeNS(fe[r],a,n):i.setAttribute(a,n);return i},input:function(e){var t=document.createElement("input");return null!=e&&(t.type=e),t},range:function(e,t,n){1===arguments.length&&(t=e,e=null);var r=document.createElement("input");return r.min=e=null==e?0:+e,r.max=t=null==t?1:+t,r.step=null==n?"any":n=+n,r.type="range",r},select:function(e){var t=document.createElement("select");return Array.prototype.forEach.call(e,(function(e){var n=document.createElement("option");n.value=n.textContent=e,t.appendChild(n)})),t},svg:function(e,t){var n=document.createElementNS("http://www.w3.org/2000/svg","svg");return n.setAttribute("viewBox",[0,0,e,t]),n.setAttribute("width",e),n.setAttribute("height",t),n},text:function(e){return document.createTextNode(e)},uid:function(e){return new he("O-"+(null==e?"":e+"-")+ ++de)}};var pe={buffer:function(e){return new Promise((function(t,n){var r=new FileReader;r.onload=function(){t(r.result)},r.onerror=n,r.readAsArrayBuffer(e)}))},text:function(e){return new Promise((function(t,n){var r=new FileReader;r.onload=function(){t(r.result)},r.onerror=n,r.readAsText(e)}))},url:function(e){return new Promise((function(t,n){var r=new FileReader;r.onload=function(){t(r.result)},r.onerror=n,r.readAsDataURL(e)}))}};function we(){return this}function ve(e,t){let n=!1;if("function"!=typeof t)throw new Error("dispose is not a function");return{[Symbol.iterator]:we,next:()=>n?{done:!0}:(n=!0,{done:!1,value:e}),return:()=>(n=!0,t(e),{done:!0}),throw:()=>({done:n=!0})}}function ye(e){let t,n,r=!1;const o=e((function(e){n?(n(e),n=null):r=!0;return t=e}));if(null!=o&&"function"!=typeof o)throw new Error("function"==typeof o.then?"async initializers are not supported":"initializer returned something, but not a dispose function");return{[Symbol.iterator]:we,throw:()=>({done:!0}),return:()=>(null!=o&&o(),{done:!0}),next:function(){return{done:!1,value:r?(r=!1,Promise.resolve(t)):new Promise((e=>n=e))}}}}function ge(e){switch(e.type){case"range":case"number":return e.valueAsNumber;case"date":return e.valueAsDate;case"checkbox":return e.checked;case"file":return e.multiple?e.files:e.files[0];case"select-multiple":return Array.from(e.selectedOptions,(e=>e.value));default:return e.value}}var be={disposable:ve,filter:function*(e,t){for(var n,r=-1;!(n=e.next()).done;)t(n.value,++r)&&(yield n.value)},input:function(e){return ye((function(t){var n=function(e){switch(e.type){case"button":case"submit":case"checkbox":return"click";case"file":return"change";default:return"input"}}(e),r=ge(e);function o(){t(ge(e))}return e.addEventListener(n,o),void 0!==r&&t(r),function(){e.removeEventListener(n,o)}}))},map:function*(e,t){for(var n,r=-1;!(n=e.next()).done;)yield t(n.value,++r)},observe:ye,queue:function(e){let t;const n=[],r=e((function(e){n.push(e),t&&(t(n.shift()),t=null);return e}));if(null!=r&&"function"!=typeof r)throw new Error("function"==typeof r.then?"async initializers are not supported":"initializer returned something, but not a dispose function");return{[Symbol.iterator]:we,throw:()=>({done:!0}),return:()=>(null!=r&&r(),{done:!0}),next:function(){return{done:!1,value:n.length?Promise.resolve(n.shift()):new Promise((e=>t=e))}}}},range:function*(e,t,n){e=+e,t=+t,n=(o=arguments.length)<2?(t=e,e=0,1):o<3?1:+n;for(var r=-1,o=0|Math.max(0,Math.ceil((t-e)/n));++r<o;)yield e+r*n},valueAt:function(e,t){if(!(!isFinite(t=+t)||t<0||t!=t|0))for(var n,r=-1;!(n=e.next()).done;)if(++r===t)return n.value},worker:function(e){const t=URL.createObjectURL(new Blob([e],{type:"text/javascript"})),n=new Worker(t);return ve(n,(()=>{n.terminate(),URL.revokeObjectURL(t)}))}};function xe(e,t){return function(n){var r,o,i,a,s,u,l,c,f=n[0],d=[],h=null,m=-1;for(s=1,u=arguments.length;s<u;++s){if((r=arguments[s])instanceof Node)d[++m]=r,f+="\x3c!--o:"+m+"--\x3e";else if(Array.isArray(r)){for(l=0,c=r.length;l<c;++l)(o=r[l])instanceof Node?(null===h&&(d[++m]=h=document.createDocumentFragment(),f+="\x3c!--o:"+m+"--\x3e"),h.appendChild(o)):(h=null,f+=o);h=null}else f+=r;f+=n[s]}if(h=e(f),++m>0){for(i=new Array(m),a=document.createTreeWalker(h,NodeFilter.SHOW_COMMENT,null,!1);a.nextNode();)o=a.currentNode,/^o:/.test(o.nodeValue)&&(i[+o.nodeValue.slice(2)]=o);for(s=0;s<m;++s)(o=i[s])&&o.parentNode.replaceChild(d[s],o)}return 1===h.childNodes.length?h.removeChild(h.firstChild):11===h.nodeType?((o=t()).appendChild(h),o):h}}var je=xe((function(e){var t=document.createElement("template");return t.innerHTML=e.trim(),document.importNode(t.content,!0)}),(function(){return document.createElement("span")}));function Ee(e){let t;Object.defineProperties(this,{generator:{value:ye((e=>{t=e}))},value:{get:()=>e,set:n=>t(e=n)}}),void 0!==e&&t(e)}function*Ae(){for(;;)yield Date.now()}var Pe=new Map;function Ce(e,t){var n;return(n=Pe.get(e=+e))?n.then((()=>t)):(n=Date.now())>=e?Promise.resolve(t):function(e,t){var n=new Promise((function(n){Pe.delete(t);var r=t-e;if(!(r>0))throw new Error("invalid time");if(r>2147483647)throw new Error("too long to wait");setTimeout(n,r)}));return Pe.set(t,n),n}(n,e).then((()=>t))}var Oe={delay:function(e,t){return new Promise((function(n){setTimeout((function(){n(t)}),e)}))},tick:function(e,t){return Ce(Math.ceil((Date.now()+1)/e)*e,t)},when:Ce};function Ne(e,t){if(/^(\w+:)|\/\//i.test(e))return e;if(/^[.]{0,2}\//i.test(e))return new URL(e,null==t?location:t).href;if(!e.length||/^[\s._]/.test(e)||/\s$/.test(e))throw new Error("illegal name");return"https://unpkg.com/"+e}function $e(e){return null==e?N:$(e)}var ke=xe((function(e){var t=document.createElementNS("http://www.w3.org/2000/svg","g");return t.innerHTML=e.trim(),t}),(function(){return document.createElementNS("http://www.w3.org/2000/svg","g")})),Le=String.raw;function Re(){return ye((function(e){var t=e(document.body.clientWidth);function n(){var n=document.body.clientWidth;n!==t&&e(t=n)}return window.addEventListener("resize",n),function(){window.removeEventListener("resize",n)}}))}var Te=Object.assign((function(e){const t=$e(e);var n;Object.defineProperties(this,(n={FileAttachment:()=>ce,Arrow:()=>t(J.resolve()),Inputs:()=>t(U.resolve()).then((e=>({...e,file:e.fileOf(le)}))),Mutable:()=>Ee,Plot:()=>t(S.resolve()),SQLite:()=>K(t),SQLiteDatabaseClient:()=>SQLiteDatabaseClient,_:()=>t(B.resolve()),aq:()=>t.alias({"apache-arrow":J.resolve()})(X.resolve()),d3:()=>t(M.resolve()),dot:()=>t(_.resolve()),htl:()=>t(z.resolve()),html:()=>je,md:()=>function(e){return e(H.resolve()).then((function(t){return xe((function(n){var r=document.createElement("div");r.innerHTML=t(n,{langPrefix:""}).trim();var o=r.querySelectorAll("pre code[class]");return o.length>0&&e(D.resolve()).then((function(t){o.forEach((function(n){function r(){t.highlightBlock(n),n.parentNode.classList.add("observablehq--md-pre")}t.getLanguage(n.className)?r():e(D.resolve("async-languages/index.js")).then((r=>{if(r.has(n.className))return e(D.resolve("async-languages/"+r.get(n.className))).then((e=>{t.registerLanguage(n.className,e)}))})).then(r,r)}))})),r}),(function(){return document.createElement("div")}))}))}(t),now:Ae,require:()=>t,resolve:()=>Ne,svg:()=>ke,tex:()=>function(e){return Promise.all([e(F.resolve()),(t=F.resolve("dist/katex.min.css"),new Promise((function(e,n){var r=document.createElement("link");r.rel="stylesheet",r.href=t,r.onerror=n,r.onload=e,document.head.appendChild(r)})))]).then((function(e){var t=e[0],n=r();function r(e){return function(){var n=document.createElement("div");return t.render(Le.apply(String,arguments),n,e),n.removeChild(n.firstChild)}}return n.options=r,n.block=r({displayMode:!0}),n}));var t}(t),topojson:()=>t(Y.resolve()),vl:()=>async function(e){const[t,n,r]=await Promise.all([I,V,Q].map((t=>e(t.resolve()))));return r.register(t,n)}(t),width:Re,DOM:me,Files:pe,Generators:be,Promises:Oe},Object.fromEntries(Object.entries(n).map(qe))))}),{resolve:N.resolve});function qe([e,t]){return[e,{value:t,writable:!0,enumerable:!0}]}e.AbstractFile=le,e.FileAttachments=function(e){return Object.assign((t=>{const n=e(t+="");if(null==n)throw new Error(`File not found: ${t}`);return new FileAttachment(n,t)}),{prototype:FileAttachment.prototype})},e.Library=Te,Object.defineProperty(e,"__esModule",{value:!0})}));