/*! For license information please see mch-image-component.198110864e2b73abf6c7.js.LICENSE.txt */
(window.webpackJsonp=window.webpackJsonp||[]).push([[54],{1e3:function(t,e,n){"use strict";n.r(e);var r=n(392),o=n(7),i=n(3),s=n(9),c=n(389),a=n(470),u=n(396),d=n(546),l=n(559),h=(n(579),function(t,e){var n={};for(var r in t)Object.prototype.hasOwnProperty.call(t,r)&&e.indexOf(r)<0&&(n[r]=t[r]);if(null!=t&&"function"==typeof Object.getOwnPropertySymbols){var o=0;for(r=Object.getOwnPropertySymbols(t);o<r.length;o++)e.indexOf(r[o])<0&&Object.prototype.propertyIsEnumerable.call(t,r[o])&&(n[r[o]]=t[r[o]])}return n});var m=t=>{const{renditions:e,src:n,alt:r="",caption:m="",hasLightbox:g=!1,isEnlarged:f=!1,maxHeight:b=!1,lightboxSendEvent:p=!1,lazy:w=!0}=t,O=h(t,["renditions","src","alt","caption","hasLightbox","isEnlarged","maxHeight","lightboxSendEvent","lazy"]),v=Object(i.createRef)(),j=p?t=>{var e;t.stopPropagation(),null===(e=v.current)||void 0===e||e.dispatchEvent(new CustomEvent("mch-image-click",{bubbles:!0,composed:!0}))}:e=>{e.stopPropagation();const o={category:"Image",parameter:"open lightbox",name:Object(l.a)(t)};Object(c.A)("analytics-track-event",{data:o}),Object(a.a)({type:"image",data:{src:n,alt:r,caption:m}})};return Object(o.jsx)("div",Object.assign({className:"mch-image "+(f?"mch-image--enlarged":""),ref:v},{children:g?Object(o.jsx)(o.Fragment,{children:Object(o.jsx)(s.c,Object.assign({kind:"clear",onClick:j,className:"mch-image__overlay-link",ariaLabel:Object(d.a)("openModal"),isFullWidth:!0},{children:Object(o.jsx)(s.g,Object.assign({className:"mch-image__overlay-image "+(f?"mch-image--enlarged":""),isEnlarged:f,maxHeight:b,src:e?void 0:n},e,{viewports:u.d,alt:r,caption:m,lazy:w},O))}))}):Object(o.jsx)(s.g,Object.assign({className:""+(f?"mch-image--enlarged":""),src:e?void 0:n},e,{isEnlarged:f,maxHeight:b,viewports:u.d,alt:r,caption:m,lazy:w},O))}))};e.default={component:m,renderer:new r.a,shadowStyles:[":host{display:block}.mch-image__overlay-link{--padding-start:0;--padding-top:0;--padding-bottom:0;--padding-end:0;--color-hover:var(--wb-ds-color-text);width:100%}.mch-image__overlay-image{width:100%}.mch-image--enlarged{display:flex;max-height:100%;justify-content:center}"]}},389:function(t,e,n){"use strict";var r=n(395);n.d(e,"a",(function(){return r.a})),n.d(e,"b",(function(){return r.b})),n.d(e,"c",(function(){return r.c})),n.d(e,"d",(function(){return r.d})),n.d(e,"e",(function(){return r.e})),n.d(e,"f",(function(){return r.f})),n.d(e,"g",(function(){return r.g})),n.d(e,"h",(function(){return r.h})),n.d(e,"i",(function(){return r.i})),n.d(e,"j",(function(){return r.j})),n.d(e,"k",(function(){return r.k})),n.d(e,"l",(function(){return r.l})),n.d(e,"m",(function(){return r.m})),n.d(e,"n",(function(){return r.n})),n.d(e,"o",(function(){return r.o})),n.d(e,"p",(function(){return r.p})),n.d(e,"q",(function(){return r.q})),n.d(e,"r",(function(){return r.r})),n.d(e,"s",(function(){return r.s})),n.d(e,"t",(function(){return r.t})),n.d(e,"u",(function(){return r.u})),n.d(e,"v",(function(){return r.v})),n.d(e,"w",(function(){return r.w})),n.d(e,"x",(function(){return r.x})),n.d(e,"y",(function(){return r.y})),n.d(e,"z",(function(){return r.z})),n.d(e,"A",(function(){return r.A})),n.d(e,"B",(function(){return r.B})),n.d(e,"C",(function(){return r.C})),n.d(e,"D",(function(){return r.D})),n.d(e,"E",(function(){return r.E})),n.d(e,"F",(function(){return r.F})),n.d(e,"G",(function(){return r.G}))},390:function(t,e,n){"use strict";n.d(e,"c",(function(){return o})),n.d(e,"b",(function(){return i})),n.d(e,"e",(function(){return s})),n.d(e,"d",(function(){return c})),n.d(e,"a",(function(){return r}));const r="mch-open-overlay",o=(t,e,n=document)=>{n.addEventListener(t,e)},i=(t,e,n=document)=>{n.removeEventListener(t,e)},s=(t,e)=>{document.dispatchEvent(new CustomEvent(t,{detail:e}))},c=(t,e,n=document)=>{const r=o=>{e(o),i(t,r,n)};o(t,r,n)}},391:function(t,e,n){"use strict";n.d(e,"a",(function(){return i}));var r=n(10),o=n(3);class i extends o.Component{constructor(t){super(t||{}),this._slot=Object(o.createRef)(),this._prevSlottedElements=new Set}componentDidMount(){const t=this._slot.current;t&&(this._slotChangeListener=e=>{this.onSlotChange(t)},t.addEventListener("slotchange",this._slotChangeListener),Object(r.updateAttribute)(t,"name",this.props.name),this.onSlotChange(t))}componentDidUpdate(){const t=this._slot.current;t&&(Object(r.updateAttribute)(t,"name",this.props.name),Object(r.updateSlottedElements)(t,this.props,s))}componentWillUnmount(){const t=this._slot.current;t&&this._slotChangeListener&&(null==t||t.removeEventListener("slotchange",this._slotChangeListener))}render(){const{name:t}=this.props;return Object(o.createElement)("slot",{ref:this._slot,...t?{name:t}:{}})}onSlotChange(t){var e;if(Object(r.updateSlottedElements)(t,this.props,s),this.props.onSlotChange){const n=null===(e=t.assignedNodes({flatten:!0}))||void 0===e?void 0:e.filter(t=>t.nodeType===Node.ELEMENT_NODE);this.slottedElementsHaveChanged(n)&&(this.props.onSlotChange(n),this._prevSlottedElements=new Set(n))}}slottedElementsHaveChanged(t){return this._prevSlottedElements.size!==t.length||void 0!==t.find(t=>!this._prevSlottedElements.has(t))}}const s=t=>"className"===t?"class":"htmlFor"===t?"for":t},392:function(t,e,n){"use strict";n.d(e,"a",(function(){return i})),n.d(e,"b",(function(){return s.a}));var r=n(3),o=n(25);class i{render(t,e,n,i,s,c,a){const u="platform-component";a&&Object(o.hydrate)(Object(r.createElement)(r.Fragment,{},Object(r.createElement)(n,{...i,key:u})),e),Object(o.render)(Object(r.createElement)(r.Fragment,{},...c.map((t,e)=>Object(r.createElement)("style",{key:"style-"+e},t)),Object(r.createElement)(n,{...i,key:u})),e)}unmount(t){Object(o.unmountComponentAtNode)(t)}}var s=n(391)},395:function(t,e,n){"use strict";n.d(e,"r",(function(){return c})),n.d(e,"o",(function(){return a})),n.d(e,"y",(function(){return u})),n.d(e,"p",(function(){return d})),n.d(e,"e",(function(){return m})),n.d(e,"h",(function(){return g})),n.d(e,"f",(function(){return f})),n.d(e,"g",(function(){return b})),n.d(e,"z",(function(){return p})),n.d(e,"n",(function(){return w})),n.d(e,"k",(function(){return O})),n.d(e,"l",(function(){return v})),n.d(e,"m",(function(){return j})),n.d(e,"i",(function(){return y})),n.d(e,"j",(function(){return E})),n.d(e,"C",(function(){return L})),n.d(e,"b",(function(){return k})),n.d(e,"a",(function(){return I})),n.d(e,"E",(function(){return C})),n.d(e,"F",(function(){return D})),n.d(e,"B",(function(){return $})),n.d(e,"D",(function(){return P})),n.d(e,"q",(function(){return r})),n.d(e,"G",(function(){return o})),n.d(e,"x",(function(){return i})),n.d(e,"s",(function(){return M})),n.d(e,"v",(function(){return _.c})),n.d(e,"u",(function(){return _.b})),n.d(e,"A",(function(){return _.e})),n.d(e,"w",(function(){return _.d})),n.d(e,"c",(function(){return _.a})),n.d(e,"t",(function(){return N})),n.d(e,"d",(function(){return F}));const r=t=>new URLSearchParams(t.startsWith("#")?t.substring(1):t),o=(t,e)=>{const n=new URL(window.location.href),o=r(n.hash);e&&e.forEach(t=>{o.delete(t)}),Object.keys(t).forEach(e=>{o.set(e,String(t[e])||"")}),n.hash=o.toString(),window.history.replaceState(window.history.state,document.title,n.toString())},i=t=>{const e=r(t),n={};for(const t of e.keys()){const r=e.get(t);n[t]=""===r?null:r}return n};var s=n(8);const c=()=>{const{auth_token:t}=i(window.location.hash);if(t)try{const e=Object.assign(Object.assign({},JSON.parse(decodeURIComponent(escape(atob(t))))),{lastRead:Date.now()});localStorage.setItem("auth",JSON.stringify(e)),o({},["auth_token"])}catch(t){console.error("authToken not a valid base64")}},a=()=>{if(Object(s.isServer)())return null;const t=localStorage.getItem("auth"),e=Date.now();if(t)try{const n=JSON.parse(t);if(n.lastRead&&e-n.lastRead>36e5)return localStorage.removeItem("auth"),window.location.reload(),null;const r=Object.assign(Object.assign({},n),{lastRead:e});return localStorage.setItem("auth",JSON.stringify(r)),Object.assign(Object.assign({},r),{allowedGroups:n.allowedGroups})}catch(t){console.error("authToken not a valid json")}return null},u=()=>{localStorage.removeItem("auth")},d=({failIfMissing:t}={failIfMissing:!1})=>{const e=null===window||void 0===window?void 0:window.SWISSTOPO_API_KEY;if(e)return String(e);if(t)throw new Error("SWISSTOPO_API_KEY not found!");console.warn("SWISSTOPO_API_KEY not found!")},l=/[+-][0-9]{2}:[0-9]{2}\b$/,h=t=>(null==t?void 0:t.length)?Date.parse(t.replace(l,"")):NaN,m=(t,e="de-CH")=>t?new Date(h(t)).toLocaleDateString(e,{year:"numeric",month:"2-digit",day:"2-digit"}):"",g=t=>{if(!t)return"";const e=new Date("string"==typeof t?h(t):t),{hour:n,minute:r}=p(e,"de",{hour:"2-digit",minute:"2-digit"});return`${n}:${r}`},f=(t,e)=>t?new Date("string"==typeof t?h(t):t).toLocaleDateString("de-CH",{year:"numeric",month:"2-digit",day:"2-digit",hour:"2-digit",minute:"2-digit",timeZone:e}):"",b=(t,e)=>t?e?`${f(t)} - ${f(e)}`:f(t):f(e),p=(t,e,n)=>{const r=new Intl.DateTimeFormat(e,n).formatToParts(t),o={};return r.forEach(t=>{o[t.type]=t.value}),o},w=(t,e)=>{const{weekday:n,year:r,month:o,day:i,hour:s,minute:c}=p(new Date(t),e,{weekday:"long",day:"2-digit",month:"2-digit",year:"numeric",hour:"2-digit",hour12:!1,minute:"2-digit",timeZone:"Europe/Zurich"});return`${n}${"fr"===e?"":","} ${i}.${o}.${r}, ${s}:${c}`},O=(t,e)=>{const{weekday:n,year:r,month:o,day:i}=p(new Date(t),e,{weekday:"long",day:"2-digit",month:"2-digit",year:"numeric",hour12:!1,timeZone:"Europe/Zurich"});return`${n}${"fr"===e?"":","} ${i}.${o}.${r}`},v=(t,e,n)=>t?e?`${w(t,n)} - ${g(e)}`:w(t,n):w(e,n),j=(t,e)=>{const n=new Date(t),r=new Date(n.getFullYear(),n.getMonth(),n.getDate(),n.getHours(),0,0),o=new Date(n.getFullYear(),n.getMonth(),n.getDate(),n.getHours()+1,0,0);return v(r.toISOString(),o.toISOString(),e)},y=(t,e)=>{const{year:n,month:r}=p(new Date(t),e,{month:"long",year:"numeric",timeZone:"Europe/Zurich"});return`${r} ${n}`},E=(t,e)=>new Intl.DateTimeFormat(e,{month:"short",timeZone:"Europe/Zurich"}).format(new Date(t));var S=n(3);const x=(t,e)=>{const n=i(t);return e?e(n):n},L=(t,{encodeMapping:e,decodeMapping:n})=>{const r=Object(S.useMemo)(()=>x(window.location.hash,n),[n]),[s,c]=Object(S.useState)(Object.assign(Object.assign({},t),r)),a=Object(S.useCallback)(t=>{c(r=>{if("function"==typeof t){const i=t(Object.assign(Object.assign({},r),x(window.location.hash,n)));return o(e?e(i):i),i}return o(e?e(t):t),t})},[c]);return Object(S.useEffect)(()=>{const t=(t,r=!1)=>{const s=i(t);c(t=>{const i=Object.assign(Object.assign({},t),n?n(s):s);return r&&o(e?e(i):i),((t,e)=>{const n=Object.keys(t),r=Object.keys(e);if(n.length!==r.length)return!1;for(const r of n)if(t[r]!==e[r])return!1;return!0})(i,t)?t:i})},r=()=>{t(window.location.hash)};return window.addEventListener("hashchange",r),t(window.location.hash,!0),()=>{window.removeEventListener("hashchange",r)}},[]),[s,a]};var _=n(390);const k="anchorListUpdate",I="anchorListReady",C=t=>{const e=t=>{const e=new CustomEvent(k,{detail:t,bubbles:!0,composed:!0});window.dispatchEvent(e)};Object(S.useEffect)(()=>{const n=t.reduce((t,e)=>{var n;const r=null===(n=e.ref)||void 0===n?void 0:n.current,o=r instanceof HTMLSlotElement&&r.assignedElements().length?r.assignedElements()[0]:r;return o&&t.push({title:e.title,id:e.id,active:!1,element:o}),t},new Array);let r=[...n];const o=()=>{e(r)};Object(_.c)(I,o,window);const i=new IntersectionObserver(()=>{const t=.05*window.innerHeight,o=[...n].map(e=>((t,e)=>{const{top:n,bottom:r}=t.element.getBoundingClientRect();let o;return o=n<=e&&r>=e?0:Math.abs(e-n),Object.assign(Object.assign({},t),{distance:o})})(e,t)).sort(({distance:t},{distance:e})=>t<e?-1:1);r=n.map(t=>Object.assign(Object.assign({},t),{active:o[0].id===t.id})),e(r)},{rootMargin:"-5% 0px -95% 0px"});return e(n),n.forEach(t=>{i.observe(t.element)}),()=>{Object(_.b)(I,o,window),i.disconnect()}},[t])},D=t=>{const e=Object(S.useRef)(null);return Object(S.useEffect)(()=>{const n=()=>{e.current&&(e.current.scrollIntoView(),e.current.tabIndex=0,e.current.focus(),e.current.addEventListener("blur",()=>{e.current&&(e.current.tabIndex=-1)}))};return Object(_.c)("mch-goto-"+t,n,window),()=>{Object(_.b)("mch-goto-"+t,n,window)}},[]),e},$=()=>{var t;const e=Object(S.useRef)(window.matchMedia("(pointer: fine)")),[n,r]=Object(S.useState)(null===(t=e.current.matches)||void 0===t||t);return Object(S.useEffect)(()=>{const t=t=>{r(t.matches)};return e.current.addEventListener("change",t),()=>e.current.removeEventListener("change",t)},[]),n},P=()=>{const[t,e]=Object(S.useState)(!1);return Object(S.useEffect)(()=>{fetch("/maintenance").then(t=>t.json()).then(t=>{e(t.maintenance)})},[]),t};function M(t){return e=>{var n,r;return null!==(r=null===(n=t[e])||void 0===n?void 0:n[Object(s.getLanguage)()])&&void 0!==r?r:e}}const N=t=>{let e=void 0,n=void 0;return r=>e===r&&n?n:(e=r,n=t(e))};class F{constructor(t,e){this.segments=t.split("/").filter(t=>""!==t.trim()),this.ext=e||this.getExtensionFromLastSegment()}getExtensionFromLastSegment(){const t=this.segments.slice(-1).filter(t=>-1!==t.indexOf(".")),e=t.map(t=>t.substring(0,t.lastIndexOf("."))).pop();return e&&(this.segments.pop(),this.segments.push(e)),t.map(t=>t.substring(t.lastIndexOf(".")+1)).pop()}skipLastSegment(){return new F(this.toPath(this.segments.slice(0,-1)))}get(){return this.toPathWithExt(this.segments)}extension(t){return 0===this.segments.length?this:new F(this.toPath(this.segments),t)}toPathWithExt(t){return`${this.toPath(t)}${this.ext?"."+this.ext:""}`}toPath(t){return"/"+t.join("/")}}},396:function(t,e,n){"use strict";var r;n.d(e,"f",(function(){return i})),n.d(e,"c",(function(){return s})),n.d(e,"b",(function(){return c})),n.d(e,"a",(function(){return a})),n.d(e,"e",(function(){return u})),n.d(e,"d",(function(){return h}));class o{constructor(t,e){this.name=t,this.minWidth=e,this[r]=()=>this.minWidth}atLeast(t){return t>=this.minWidth}}r=Symbol.toPrimitive;const i=new o("xs",0),s=new o("sm",480),c=new o("md",768),a=new o("lg",1024),u=new o("xl",1280),d=new o("xxl",1680),l=[i,s,c,a,u,d],h=(l.map(t=>t.name),l.map(t=>[t.name,t.minWidth]))},470:function(t,e,n){"use strict";n.d(e,"a",(function(){return o}));var r=n(390);const o=t=>{Object(r.e)(r.a,Object.assign(Object.assign({},t),{data:Object.assign(Object.assign({},t.data),{scrollPosition:window.scrollY})}))}},546:function(t,e,n){"use strict";n.d(e,"a",(function(){return o}));var r=n(389);const o=Object(r.s)({openModal:{de:"schaue das Bild in Grossansicht an",fr:"voir l'image en grand format",it:"Ingrandire l'immagine",en:"View the image full-screen"},closeModal:{de:"schliesse die Lightbox",fr:"fermer la lightbox",it:"chiudi la lightbox",en:"close the Lightbox"}})},559:function(t,e,n){"use strict";function r(t){var e;const n=null===(e=t.renditions)||void 0===e?void 0:e.md;if(n){const e=n.split(",");return 2==e.length?e[0]:t.src}return t.src}n.d(e,"a",(function(){return r}))},579:function(t,e){}}]);