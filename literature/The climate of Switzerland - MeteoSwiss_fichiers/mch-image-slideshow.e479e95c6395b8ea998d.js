(window.webpackJsonp=window.webpackJsonp||[]).push([[56],{389:function(t,e,n){"use strict";var i=n(395);n.d(e,"a",(function(){return i.a})),n.d(e,"b",(function(){return i.b})),n.d(e,"c",(function(){return i.c})),n.d(e,"d",(function(){return i.d})),n.d(e,"e",(function(){return i.e})),n.d(e,"f",(function(){return i.f})),n.d(e,"g",(function(){return i.g})),n.d(e,"h",(function(){return i.h})),n.d(e,"i",(function(){return i.i})),n.d(e,"j",(function(){return i.j})),n.d(e,"k",(function(){return i.k})),n.d(e,"l",(function(){return i.l})),n.d(e,"m",(function(){return i.m})),n.d(e,"n",(function(){return i.n})),n.d(e,"o",(function(){return i.o})),n.d(e,"p",(function(){return i.p})),n.d(e,"q",(function(){return i.q})),n.d(e,"r",(function(){return i.r})),n.d(e,"s",(function(){return i.s})),n.d(e,"t",(function(){return i.t})),n.d(e,"u",(function(){return i.u})),n.d(e,"v",(function(){return i.v})),n.d(e,"w",(function(){return i.w})),n.d(e,"x",(function(){return i.x})),n.d(e,"y",(function(){return i.y})),n.d(e,"z",(function(){return i.z})),n.d(e,"A",(function(){return i.A})),n.d(e,"B",(function(){return i.B})),n.d(e,"C",(function(){return i.C})),n.d(e,"D",(function(){return i.D})),n.d(e,"E",(function(){return i.E})),n.d(e,"F",(function(){return i.F})),n.d(e,"G",(function(){return i.G}))},390:function(t,e,n){"use strict";n.d(e,"c",(function(){return r})),n.d(e,"b",(function(){return s})),n.d(e,"e",(function(){return o})),n.d(e,"d",(function(){return c})),n.d(e,"a",(function(){return i}));const i="mch-open-overlay",r=(t,e,n=document)=>{n.addEventListener(t,e)},s=(t,e,n=document)=>{n.removeEventListener(t,e)},o=(t,e)=>{document.dispatchEvent(new CustomEvent(t,{detail:e}))},c=(t,e,n=document)=>{const i=r=>{e(r),s(t,i,n)};r(t,i,n)}},395:function(t,e,n){"use strict";n.d(e,"r",(function(){return c})),n.d(e,"o",(function(){return a})),n.d(e,"y",(function(){return d})),n.d(e,"p",(function(){return l})),n.d(e,"e",(function(){return h})),n.d(e,"h",(function(){return g})),n.d(e,"f",(function(){return f})),n.d(e,"g",(function(){return b})),n.d(e,"z",(function(){return w})),n.d(e,"n",(function(){return p})),n.d(e,"k",(function(){return v})),n.d(e,"l",(function(){return k})),n.d(e,"m",(function(){return j})),n.d(e,"i",(function(){return O})),n.d(e,"j",(function(){return x})),n.d(e,"C",(function(){return _})),n.d(e,"b",(function(){return I})),n.d(e,"a",(function(){return L})),n.d(e,"E",(function(){return D})),n.d(e,"F",(function(){return $})),n.d(e,"B",(function(){return P})),n.d(e,"D",(function(){return z})),n.d(e,"q",(function(){return i})),n.d(e,"G",(function(){return r})),n.d(e,"x",(function(){return s})),n.d(e,"s",(function(){return C})),n.d(e,"v",(function(){return S.c})),n.d(e,"u",(function(){return S.b})),n.d(e,"A",(function(){return S.e})),n.d(e,"w",(function(){return S.d})),n.d(e,"c",(function(){return S.a})),n.d(e,"t",(function(){return N})),n.d(e,"d",(function(){return R}));const i=t=>new URLSearchParams(t.startsWith("#")?t.substring(1):t),r=(t,e)=>{const n=new URL(window.location.href),r=i(n.hash);e&&e.forEach(t=>{r.delete(t)}),Object.keys(t).forEach(e=>{r.set(e,String(t[e])||"")}),n.hash=r.toString(),window.history.replaceState(window.history.state,document.title,n.toString())},s=t=>{const e=i(t),n={};for(const t of e.keys()){const i=e.get(t);n[t]=""===i?null:i}return n};var o=n(8);const c=()=>{const{auth_token:t}=s(window.location.hash);if(t)try{const e=Object.assign(Object.assign({},JSON.parse(decodeURIComponent(escape(atob(t))))),{lastRead:Date.now()});localStorage.setItem("auth",JSON.stringify(e)),r({},["auth_token"])}catch(t){console.error("authToken not a valid base64")}},a=()=>{if(Object(o.isServer)())return null;const t=localStorage.getItem("auth"),e=Date.now();if(t)try{const n=JSON.parse(t);if(n.lastRead&&e-n.lastRead>36e5)return localStorage.removeItem("auth"),window.location.reload(),null;const i=Object.assign(Object.assign({},n),{lastRead:e});return localStorage.setItem("auth",JSON.stringify(i)),Object.assign(Object.assign({},i),{allowedGroups:n.allowedGroups})}catch(t){console.error("authToken not a valid json")}return null},d=()=>{localStorage.removeItem("auth")},l=({failIfMissing:t}={failIfMissing:!1})=>{const e=null===window||void 0===window?void 0:window.SWISSTOPO_API_KEY;if(e)return String(e);if(t)throw new Error("SWISSTOPO_API_KEY not found!");console.warn("SWISSTOPO_API_KEY not found!")},u=/[+-][0-9]{2}:[0-9]{2}\b$/,m=t=>(null==t?void 0:t.length)?Date.parse(t.replace(u,"")):NaN,h=(t,e="de-CH")=>t?new Date(m(t)).toLocaleDateString(e,{year:"numeric",month:"2-digit",day:"2-digit"}):"",g=t=>{if(!t)return"";const e=new Date("string"==typeof t?m(t):t),{hour:n,minute:i}=w(e,"de",{hour:"2-digit",minute:"2-digit"});return`${n}:${i}`},f=(t,e)=>t?new Date("string"==typeof t?m(t):t).toLocaleDateString("de-CH",{year:"numeric",month:"2-digit",day:"2-digit",hour:"2-digit",minute:"2-digit",timeZone:e}):"",b=(t,e)=>t?e?`${f(t)} - ${f(e)}`:f(t):f(e),w=(t,e,n)=>{const i=new Intl.DateTimeFormat(e,n).formatToParts(t),r={};return i.forEach(t=>{r[t.type]=t.value}),r},p=(t,e)=>{const{weekday:n,year:i,month:r,day:s,hour:o,minute:c}=w(new Date(t),e,{weekday:"long",day:"2-digit",month:"2-digit",year:"numeric",hour:"2-digit",hour12:!1,minute:"2-digit",timeZone:"Europe/Zurich"});return`${n}${"fr"===e?"":","} ${s}.${r}.${i}, ${o}:${c}`},v=(t,e)=>{const{weekday:n,year:i,month:r,day:s}=w(new Date(t),e,{weekday:"long",day:"2-digit",month:"2-digit",year:"numeric",hour12:!1,timeZone:"Europe/Zurich"});return`${n}${"fr"===e?"":","} ${s}.${r}.${i}`},k=(t,e,n)=>t?e?`${p(t,n)} - ${g(e)}`:p(t,n):p(e,n),j=(t,e)=>{const n=new Date(t),i=new Date(n.getFullYear(),n.getMonth(),n.getDate(),n.getHours(),0,0),r=new Date(n.getFullYear(),n.getMonth(),n.getDate(),n.getHours()+1,0,0);return k(i.toISOString(),r.toISOString(),e)},O=(t,e)=>{const{year:n,month:i}=w(new Date(t),e,{month:"long",year:"numeric",timeZone:"Europe/Zurich"});return`${i} ${n}`},x=(t,e)=>new Intl.DateTimeFormat(e,{month:"short",timeZone:"Europe/Zurich"}).format(new Date(t));var y=n(3);const E=(t,e)=>{const n=s(t);return e?e(n):n},_=(t,{encodeMapping:e,decodeMapping:n})=>{const i=Object(y.useMemo)(()=>E(window.location.hash,n),[n]),[o,c]=Object(y.useState)(Object.assign(Object.assign({},t),i)),a=Object(y.useCallback)(t=>{c(i=>{if("function"==typeof t){const s=t(Object.assign(Object.assign({},i),E(window.location.hash,n)));return r(e?e(s):s),s}return r(e?e(t):t),t})},[c]);return Object(y.useEffect)(()=>{const t=(t,i=!1)=>{const o=s(t);c(t=>{const s=Object.assign(Object.assign({},t),n?n(o):o);return i&&r(e?e(s):s),((t,e)=>{const n=Object.keys(t),i=Object.keys(e);if(n.length!==i.length)return!1;for(const i of n)if(t[i]!==e[i])return!1;return!0})(s,t)?t:s})},i=()=>{t(window.location.hash)};return window.addEventListener("hashchange",i),t(window.location.hash,!0),()=>{window.removeEventListener("hashchange",i)}},[]),[o,a]};var S=n(390);const I="anchorListUpdate",L="anchorListReady",D=t=>{const e=t=>{const e=new CustomEvent(I,{detail:t,bubbles:!0,composed:!0});window.dispatchEvent(e)};Object(y.useEffect)(()=>{const n=t.reduce((t,e)=>{var n;const i=null===(n=e.ref)||void 0===n?void 0:n.current,r=i instanceof HTMLSlotElement&&i.assignedElements().length?i.assignedElements()[0]:i;return r&&t.push({title:e.title,id:e.id,active:!1,element:r}),t},new Array);let i=[...n];const r=()=>{e(i)};Object(S.c)(L,r,window);const s=new IntersectionObserver(()=>{const t=.05*window.innerHeight,r=[...n].map(e=>((t,e)=>{const{top:n,bottom:i}=t.element.getBoundingClientRect();let r;return r=n<=e&&i>=e?0:Math.abs(e-n),Object.assign(Object.assign({},t),{distance:r})})(e,t)).sort(({distance:t},{distance:e})=>t<e?-1:1);i=n.map(t=>Object.assign(Object.assign({},t),{active:r[0].id===t.id})),e(i)},{rootMargin:"-5% 0px -95% 0px"});return e(n),n.forEach(t=>{s.observe(t.element)}),()=>{Object(S.b)(L,r,window),s.disconnect()}},[t])},$=t=>{const e=Object(y.useRef)(null);return Object(y.useEffect)(()=>{const n=()=>{e.current&&(e.current.scrollIntoView(),e.current.tabIndex=0,e.current.focus(),e.current.addEventListener("blur",()=>{e.current&&(e.current.tabIndex=-1)}))};return Object(S.c)("mch-goto-"+t,n,window),()=>{Object(S.b)("mch-goto-"+t,n,window)}},[]),e},P=()=>{var t;const e=Object(y.useRef)(window.matchMedia("(pointer: fine)")),[n,i]=Object(y.useState)(null===(t=e.current.matches)||void 0===t||t);return Object(y.useEffect)(()=>{const t=t=>{i(t.matches)};return e.current.addEventListener("change",t),()=>e.current.removeEventListener("change",t)},[]),n},z=()=>{const[t,e]=Object(y.useState)(!1);return Object(y.useEffect)(()=>{fetch("/maintenance").then(t=>t.json()).then(t=>{e(t.maintenance)})},[]),t};function C(t){return e=>{var n,i;return null!==(i=null===(n=t[e])||void 0===n?void 0:n[Object(o.getLanguage)()])&&void 0!==i?i:e}}const N=t=>{let e=void 0,n=void 0;return i=>e===i&&n?n:(e=i,n=t(e))};class R{constructor(t,e){this.segments=t.split("/").filter(t=>""!==t.trim()),this.ext=e||this.getExtensionFromLastSegment()}getExtensionFromLastSegment(){const t=this.segments.slice(-1).filter(t=>-1!==t.indexOf(".")),e=t.map(t=>t.substring(0,t.lastIndexOf("."))).pop();return e&&(this.segments.pop(),this.segments.push(e)),t.map(t=>t.substring(t.lastIndexOf(".")+1)).pop()}skipLastSegment(){return new R(this.toPath(this.segments.slice(0,-1)))}get(){return this.toPathWithExt(this.segments)}extension(t){return 0===this.segments.length?this:new R(this.toPath(this.segments),t)}toPathWithExt(t){return`${this.toPath(t)}${this.ext?"."+this.ext:""}`}toPath(t){return"/"+t.join("/")}}},396:function(t,e,n){"use strict";var i;n.d(e,"f",(function(){return s})),n.d(e,"c",(function(){return o})),n.d(e,"b",(function(){return c})),n.d(e,"a",(function(){return a})),n.d(e,"e",(function(){return d})),n.d(e,"d",(function(){return m}));class r{constructor(t,e){this.name=t,this.minWidth=e,this[i]=()=>this.minWidth}atLeast(t){return t>=this.minWidth}}i=Symbol.toPrimitive;const s=new r("xs",0),o=new r("sm",480),c=new r("md",768),a=new r("lg",1024),d=new r("xl",1280),l=new r("xxl",1680),u=[s,o,c,a,d,l],m=(u.map(t=>t.name),u.map(t=>[t.name,t.minWidth]))},404:function(t,e,n){"use strict";n.d(e,"a",(function(){return o}));var i=n(3),r=n(10);n(433);const s=t=>"string"==typeof t?t:JSON.stringify(t),o=(t,e=!0)=>n=>Object(i.createElement)(t,Object.entries(n).reduce((t,[e,n])=>{switch(e){case"className":return Object.assign(Object.assign({},t),{class:s(n)});case"children":return t;default:return Object.assign(Object.assign({},t),{[Object(r.propertyNameToAttributeName)(e)]:s(n)})}},e?{"qs-state":"unresolved"}:{}),"children"in n?n.children:[])},433:function(t,e){},470:function(t,e,n){"use strict";n.d(e,"a",(function(){return r}));var i=n(390);const r=t=>{Object(i.e)(i.a,Object.assign(Object.assign({},t),{data:Object.assign(Object.assign({},t.data),{scrollPosition:window.scrollY})}))}},546:function(t,e,n){"use strict";n.d(e,"a",(function(){return r}));var i=n(389);const r=Object(i.s)({openModal:{de:"schaue das Bild in Grossansicht an",fr:"voir l'image en grand format",it:"Ingrandire l'immagine",en:"View the image full-screen"},closeModal:{de:"schliesse die Lightbox",fr:"fermer la lightbox",it:"chiudi la lightbox",en:"close the Lightbox"}})},554:function(t,e,n){"use strict";n.d(e,"a",(function(){return i}));const i="mch-image-loaded"},559:function(t,e,n){"use strict";function i(t){var e;const n=null===(e=t.renditions)||void 0===e?void 0:e.md;if(n){const e=n.split(",");return 2==e.length?e[0]:t.src}return t.src}n.d(e,"a",(function(){return i}))},579:function(t,e){},811:function(t,e){},943:function(t,e,n){"use strict";n.r(e);var i=n(392),r=n(7),s=n(615),o=n.n(s),c=n(3),a=n(9),d=n(389);const l=Object(d.s)({nextImage:{de:"zum nächsten Bild",fr:"image suivante",it:"immagine successiva",en:"next Image"},prevImage:{de:"zum vorherigen Bild",fr:"image précédente",it:"immagine precedente",en:"previous Image"}});var u=n(470),m=n(554),h=(n(396),n(546),n(559));n(579);var g=n(404);const f=Object(g.a)("mch-image-component"),b=({image:{renditions:t,src:e,alt:n,caption:i},images:s,index:o,hasLightbox:a,isEnlarged:l,maxHeight:h,syncHandler:g})=>{const b=Object(c.createRef)(),[w,p]=Object(c.useState)(!0);return Object(c.useEffect)(()=>{if(b.current){const t=t=>{t.stopPropagation();const n={category:"Image",parameter:"open lightbox",name:e};Object(d.A)("analytics-track-event",{data:n}),Object(u.a)({type:"image-slideshow",data:{images:s.map(t=>({src:t.src,alt:t.alt,caption:t.caption})),index:o,handler:g}})},n=t=>{t.stopPropagation(),p(!1)};return b.current.addEventListener("mch-image-click",t),b.current.addEventListener(m.a,n),()=>{var e,i;null===(e=b.current)||void 0===e||e.removeEventListener("mch-image-click",t),null===(i=b.current)||void 0===i||i.removeEventListener(m.a,n)}}},[b.current]),Object(r.jsx)("div",Object.assign({className:"mch-image-slideshow__item "+(w?"mch-image-slideshow__item--loading":""),ref:b},{children:Object(r.jsx)(f,{src:e||"",renditions:t,maxHeight:h,alt:n,caption:i,hasLightbox:a,isEnlarged:l,lightboxSendEvent:!0,isFullWidth:!1,dispatchLoadEvent:!0,lazy:!1})}))};var w=n(8);n(811);const p=Object.freeze({dots:!0,infinite:!0,arrows:!1,speed:400,slidesToShow:1,slidesToScroll:1,accessibility:!0,draggable:!1,customPaging:t=>Object(r.jsx)("button",{type:"button","data-nav":t,"aria-label":"Carousel Page "+(t+1),tabIndex:-1},t),appendDots:t=>Object(r.jsx)("ul",Object.assign({className:"slick-dots","aria-label":"Carousel Pagination"},{children:t}))});e.default={component:({images:t=[],hasLightbox:e=!1,startIndex:n,maxHeight:i=!0,isEnlarged:s=!1})=>{const u=Object(c.useRef)(null),m=Object(c.useRef)(null);Object(c.useEffect)(()=>{var t;const e=()=>{};return null===(t=null===document||void 0===document?void 0:document.body)||void 0===t||t.addEventListener("touchstart",e),function(){var t;null===(t=null===document||void 0===document?void 0:document.body)||void 0===t||t.removeEventListener("touchstart",e)}});const g=t=>{var e;null===(e=u.current)||void 0===e||e.slickGoTo(t)};return Object(r.jsxs)("div",Object.assign({className:"mch-image-slideshow",ref:m},{children:[Object(w.isClient)()&&Object(r.jsx)(o.a,Object.assign({},p,{afterChange:e=>{var n;if(null===(n=m.current)||void 0===n||n.dispatchEvent(new CustomEvent("indexChanged",{bubbles:!0,composed:!0,detail:e})),!s){const n={category:"Image",parameter:"slide image slider",name:Object(h.a)(t[e])};Object(d.A)("analytics-track-event",{data:n})}},initialSlide:n||0,ref:u},{children:t.map((n,o)=>Object(r.jsx)(b,{image:n,images:t,index:o,maxHeight:i,isEnlarged:s,hasLightbox:e,syncHandler:g},o))})),t.length>1&&Object(r.jsxs)(r.Fragment,{children:[Object(r.jsx)(a.c,Object.assign({ariaLabel:l("prevImage"),kind:"clear",className:"mch-image-slideshow__controls mch-image-slideshow__controls--prev",onClick:()=>{var t;null===(t=u.current)||void 0===t||t.slickPrev()}},{children:Object(r.jsx)(a.f,{name:"chevron-left",size:"2xl"})})),Object(r.jsx)(a.c,Object.assign({ariaLabel:l("nextImage"),kind:"clear",className:"mch-image-slideshow__controls mch-image-slideshow__controls--next",onClick:()=>{var t;null===(t=u.current)||void 0===t||t.slickNext()}},{children:Object(r.jsx)(a.f,{name:"chevron-right",size:"2xl"})}))]})]}))},renderer:new i.a,shadowStyles:['.mch-image__overlay-link{--padding-start:0;--padding-top:0;--padding-bottom:0;--padding-end:0;--color-hover:var(--wb-ds-color-text);width:100%}.mch-image__overlay-image{width:100%}.mch-image--enlarged{display:flex;max-height:100%;justify-content:center}[qs-state=unresolved]{display:block;opacity:0}.slick-slider{box-sizing:border-box;-webkit-user-select:none;-moz-user-select:none;-ms-user-select:none;user-select:none;-webkit-touch-callout:none;-khtml-user-select:none;-ms-touch-action:pan-y;touch-action:pan-y;-webkit-tap-highlight-color:transparent}.slick-list,.slick-slider{position:relative;display:block}.slick-list{overflow:hidden;margin:0;padding:0}.slick-list:focus{outline:none}.slick-list.dragging{cursor:pointer;cursor:hand}.slick-slider .slick-list,.slick-slider .slick-track{-webkit-transform:translateZ(0);-moz-transform:translateZ(0);-ms-transform:translateZ(0);-o-transform:translateZ(0);transform:translateZ(0)}.slick-track{position:relative;top:0;left:0;display:block;margin-left:auto;margin-right:auto}.slick-track:after,.slick-track:before{display:table;content:""}.slick-track:after{clear:both}.slick-loading .slick-track{visibility:hidden}.slick-slide{display:none;float:left;height:100%;min-height:1px}[dir=rtl] .slick-slide{float:right}.slick-slide img{display:block}.slick-slide.slick-loading img{display:none}.slick-slide.dragging img{pointer-events:none}.slick-initialized .slick-slide{display:block}.slick-loading .slick-slide{visibility:hidden}.slick-vertical .slick-slide{display:block;height:auto;border:1px solid transparent}.slick-arrow.slick-hidden{display:none}:host{display:block;--mch-image-slideshow__dots-size:var(--wb-ds-space--xs);--mch-image-slideshow__dots-space:var(--wb-ds-space--s)}.mch-image-slideshow{background-color:#f0f4f7;background-color:var(--mch-color-background);padding:2.8rem;padding:var(--mch-space--boxes);position:relative;max-height:100%;max-width:100%;box-sizing:border-box}.mch-image-slideshow .slick-slider{display:flex;flex-direction:column;max-height:100%}.mch-image-slideshow .slick-slider .slick-dots{list-style:none;display:flex;justify-content:center;margin-left:calc(var(--mch-image-slideshow__dots-space)*-1);margin-top:1.6rem;margin-top:var(--wb-ds-space--s);margin-bottom:0;padding:0}.mch-image-slideshow .slick-slider .slick-dots button{width:var(--mch-image-slideshow__dots-size);height:var(--mch-image-slideshow__dots-size);border:1px solid #1f2937;border:solid var(--wb-ds-border-width--default) var(--wb-ds-color-text);margin-left:var(--mch-image-slideshow__dots-space);border-radius:50%;border-radius:var(--wb-ds-border-radius--full);padding:0;background-color:transparent;cursor:pointer}.mch-image-slideshow .slick-slider .slick-dots button:focus-visible{box-shadow:inset 0 0 0 3px #c4b5fd;box-shadow:var(--wb-ds-box-shadow--2x-hard);outline:none}@media(min-width:1024px){.mch-image-slideshow .slick-slider .slick-dots button:hover{background-color:#1f2937;background-color:var(--wb-ds-color-text)}}.mch-image-slideshow .slick-slider .slick-dots .slick-active button{background-color:#1f2937;background-color:var(--wb-ds-color-text)}@media(min-width:480px){.mch-image-slideshow .slick-slider .slick-dots{margin-top:3.2rem;margin-top:var(--wb-ds-space--2xl)}}.mch-image-slideshow__controls{position:absolute;top:50%;transform:translateY(-50%);display:none}@media(min-width:1280px){.mch-image-slideshow__controls{display:block}}.mch-image-slideshow__controls--prev{left:.8rem;left:var(--wb-ds-space--2xs)}.mch-image-slideshow__controls--next{right:.8rem;right:var(--wb-ds-space--2xs)}.mch-image-slideshow__item{transition:opacity .25s cubic-bezier(.4,0,.2,1);transition:opacity var(--wb-ds-animation-duration--default) var(--wb-ds-animation-timing-function)}.mch-image-slideshow__item--loading{opacity:0}']}}}]);