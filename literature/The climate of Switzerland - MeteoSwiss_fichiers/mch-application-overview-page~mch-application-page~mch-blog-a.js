(window.webpackJsonp=window.webpackJsonp||[]).push([[1],{394:function(e,t,n){"use strict";n.d(t,"a",(function(){return i}));const i=(...e)=>e.map(e=>"object"==typeof e?Object.entries(e).map(([e,t])=>t?e:void 0).filter(e=>!!e).join(" "):e).filter(e=>e).join(" ")},396:function(e,t,n){"use strict";var i;n.d(t,"f",(function(){return s})),n.d(t,"c",(function(){return o})),n.d(t,"b",(function(){return c})),n.d(t,"a",(function(){return r})),n.d(t,"e",(function(){return l})),n.d(t,"d",(function(){return b}));class a{constructor(e,t){this.name=e,this.minWidth=t,this[i]=()=>this.minWidth}atLeast(e){return e>=this.minWidth}}i=Symbol.toPrimitive;const s=new a("xs",0),o=new a("sm",480),c=new a("md",768),r=new a("lg",1024),l=new a("xl",1280),d=new a("xxl",1680),u=[s,o,c,r,l,d],b=(u.map(e=>e.name),u.map(e=>[e.name,e.minWidth]))},397:function(e,t,n){"use strict";n.d(t,"a",(function(){return o}));var i=n(3),a=n(10);n(411);const s=e=>"string"==typeof e?e:JSON.stringify(e),o=(e,t=!0,n={})=>{const o=(e,t)=>n[e]&&"function"==typeof t;return Object(i.memo)(c=>{const r=Object(i.useRef)(null),l=Object(i.useMemo)(()=>{var e;return null!==(e=c.elementRef)&&void 0!==e?e:r},[c.elementRef]);Object(i.useEffect)(()=>{const e=null==l?void 0:l.current,t=Object.entries(c).filter(([e,t])=>o(e,t)).map(([t,i])=>{const a=n[t],s=i;return null==e||e.addEventListener(a,s),()=>null==e?void 0:e.removeEventListener(a,s)});return()=>t.forEach(e=>e())},[c]);const d=Object.assign(Object.assign({},t?{"qs-state":"unresolved"}:{}),{ref:l});return Object(i.createElement)(e,Object.entries(c).filter(([e,t])=>!o(e,t)&&"elementRef"!==e).reduce((e,[t,n])=>{switch(t){case"className":return Object.assign(Object.assign({},e),{class:s(n)});case"children":return e;default:return Object.assign(Object.assign({},e),{[Object(a.propertyNameToAttributeName)(t)]:s(n)})}},d),"children"in c?c.children:[])})}},399:function(e,t,n){"use strict";var i=n(7),a=n(3),s=n(394),o=(n(410),function(e,t){var n={};for(var i in e)Object.prototype.hasOwnProperty.call(e,i)&&t.indexOf(i)<0&&(n[i]=e[i]);if(null!=e&&"function"==typeof Object.getOwnPropertySymbols){var a=0;for(i=Object.getOwnPropertySymbols(e);a<i.length;a++)t.indexOf(i[a])<0&&Object.prototype.propertyIsEnumerable.call(e,i[a])&&(n[i[a]]=e[i[a]])}return n});const c=Object(a.forwardRef)((e,t)=>{var{children:n,icon:a,className:c,flat:r,selected:l,small:d,iconPosition:u,noShadow:b,levelColor:h,iconSize:j=(h?"small":"normal")}=e,p=o(e,["children","icon","className","flat","selected","small","iconPosition","noShadow","levelColor","iconSize"]);return Object(i.jsxs)("button",Object.assign({},p,{ref:t,className:Object(s.a)("button-pattern",{"button--flat":r,"button--selected":l,"button--reversed":"end"===u,"button--no-shadow":b,"button--small":d},c)},{children:[a?Object(i.jsx)("svg",Object.assign({className:Object(s.a)("button__icon",{["button__icon--"+j]:!!a&&!!j})},{children:Object(i.jsx)("use",{href:"/static/resources/design-system-icons/icons.svg#"+a})})):null,n?Object(i.jsx)("div",Object.assign({className:"button__text"},{children:n})):null,h?Object(i.jsx)("div",{className:"button__level-indicator",style:{background:h}}):null]}))});t.a=c},404:function(e,t,n){"use strict";n.d(t,"a",(function(){return o}));var i=n(3),a=n(10);n(433);const s=e=>"string"==typeof e?e:JSON.stringify(e),o=(e,t=!0)=>n=>Object(i.createElement)(e,Object.entries(n).reduce((e,[t,n])=>{switch(t){case"className":return Object.assign(Object.assign({},e),{class:s(n)});case"children":return e;default:return Object.assign(Object.assign({},e),{[Object(a.propertyNameToAttributeName)(t)]:s(n)})}},t?{"qs-state":"unresolved"}:{}),"children"in n?n.children:[])},410:function(e,t){},411:function(e,t){},423:function(e,t,n){"use strict";var i=n(7),a=n(3),s=n(396),o=n(8);n(434);var c=function(e,t){var n={};for(var i in e)Object.prototype.hasOwnProperty.call(e,i)&&t.indexOf(i)<0&&(n[i]=e[i]);if(null!=e&&"function"==typeof Object.getOwnPropertySymbols){var a=0;for(i=Object.getOwnPropertySymbols(e);a<i.length;a++)t.indexOf(i[a])<0&&Object.prototype.propertyIsEnumerable.call(e,i[a])&&(n[i[a]]=e[i[a]])}return n};t.a=e=>{var{direction:t="vertical",twoColumns:n,children:r,className:l,wrap:d="wrap"}=e,u=c(e,["direction","twoColumns","children","className","wrap"]);const b="horizontal"===t?" mch-list--horizontal":"",h="nowrap"===d?" mch-list--nowrap":"",j=n?" mch-list--two-columns":"",p=function(){const[e,t]=Object(a.useState)(s.f);return Object(a.useEffect)(()=>{if(Object(o.isServer)())return;const e=()=>{window.innerWidth<s.c.minWidth?t(s.f):window.innerWidth<s.b.minWidth?t(s.c):window.innerWidth<s.a.minWidth?t(s.b):window.innerWidth<s.e.minWidth?t(s.a):t(s.e)};return e(),window.addEventListener("resize",e),()=>{window.removeEventListener("resize",e)}},[]),e}()<s.c,m=a.Children.toArray(r),f=Object(a.useMemo)(()=>{const e=Math.ceil(m.length/2),t=m.slice(0,e);return m.slice(e).forEach((e,n)=>{t.splice(2*n+1,0,e)}),t},[m]),v=n&&!p?f:m;return Object(i.jsx)("div",Object.assign({className:`${l?l+" ":""}mch-list${b}${j}${h}`},u,{children:Object(i.jsx)("ul",Object.assign({className:"mch-list__list"},{children:v.map((e,t)=>Object(i.jsx)("li",{children:e},t))}))}))}},433:function(e,t){},434:function(e,t){},441:function(e,t,n){"use strict";n.d(t,"a",(function(){return a}));var i=n(7);n(510);const a=({title:e})=>Object(i.jsx)(i.Fragment,{children:e?Object(i.jsx)("h2",Object.assign({className:"a11y-description--hidden"},{children:e})):null})},443:function(e,t,n){"use strict";var i=n(7),a=n(398),s=n.n(a);n(545);t.a=({children:e,className:t})=>Object(i.jsx)("div",Object.assign({className:s()("mch-container",t)},{children:e}))},469:function(e,t,n){"use strict";n.d(t,"a",(function(){return s}));var i=n(419);const a=Object(i.a)("persistentMyApplications"),s=e=>{const[t,n]=a(e);return{persistentMyApplications:t,toggleApplication:e=>{n(t=>t.findIndex(t=>t===e)>-1?t.filter(t=>t!==e):[e,...t])}}}},471:function(e,t,n){"use strict";n.d(t,"a",(function(){return i}));const i=e=>new URL(e,"https://quatico.com").pathname},472:function(e,t,n){"use strict";n.d(t,"a",(function(){return a}));var i=n(389);const a=Object(i.s)({mainContent:{de:"Inhaltsbereich",fr:"Contenu",it:"Ambito dei contenuti",en:"Contents area"},footer:{de:"Fussbereich",fr:"Pied de page",it:"Piè di pagina",en:"Footer"}})},490:function(e,t,n){"use strict";n.d(t,"a",(function(){return A}));var i=n(7),a=n(3),s=n(9),o=n(389),c=n(396),r=n(404);const l=Object(r.a)("mch-image-slideshow");var d=n(443),u=n(546);n(609);const b=Object(a.forwardRef)(({children:e,handleClose:t,clickEverywhereToClose:n=!1,extraClassName:a=""},o)=>Object(i.jsx)("div",Object.assign({role:"dialog","aria-modal":"true",className:["mch-overlay",a,""+(n?"mch-overlay--click-close":"")].join(" "),onClick:n?t:void 0,ref:o},{children:Object(i.jsxs)(d.a,{children:[Object(i.jsx)("div",Object.assign({className:"mch-overlay__header"},{children:Object(i.jsx)(s.c,Object.assign({className:"mch-overlay__close",ariaLabel:Object(u.a)("closeModal"),kind:"clear",onClick:n?void 0:t},{children:Object(i.jsx)(s.f,{name:"cancel",size:"2xl"})}))})),Object(i.jsx)("div",Object.assign({className:"mch-overlay__content"},{children:e}))]})})));var h=n(469),j=n(399),p=n(471),m=n(606);const f=Object(o.s)({close:{de:"Schliessen",en:"Close",it:"Chiudere",fr:"Fermer"},addToMyApplications:{de:"Zu «Meine Applikationen» hinzufügen",en:"Add to «My Applications»",fr:"Ajouter à «Mes applications»",it:"Aggiungere a «Le mie applicazioni»"},removeFromMyApplications:{de:"Von «Meine Applikationen» entfernen",en:"Remove from «My Applications»",fr:"Retirer de «Mes applications»",it:"Eliminare da «Le mie applicazioni»"}});n(772);var v=function(e,t,n,i){return new(n||(n=Promise))((function(a,s){function o(e){try{r(i.next(e))}catch(e){s(e)}}function c(e){try{r(i.throw(e))}catch(e){s(e)}}function r(e){var t;e.done?a(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(o,c)}r((i=i.apply(e,t||[])).next())}))};const O=({title:e,lead:t,close:n,url:c,setDialogState:r})=>{const[l,u]=Object(a.useState)(""),[b,O]=Object(a.useState)(""),{persistentMyApplications:g,toggleApplication:w}=Object(h.a)([]),y=Object(p.a)(c),x=g.findIndex(e=>e===y)>-1;Object(a.useEffect)(()=>{const t=document.body.style.overflow;return document.body.style.overflow="hidden",Object(o.A)("analytics-set-application-context",{data:{applicationName:e}}),v(void 0,void 0,void 0,(function*(){var e,t,n,i;const a=yield fetch(c),s=(new DOMParser).parseFromString(yield a.text(),"text/html");u(null!==(t=null===(e=s.querySelector('mch-application-page > [slot="main"], mch-external-application-page > [slot="main"]'))||void 0===e?void 0:e.outerHTML)&&void 0!==t?t:""),O(null!==(i=null===(n=s.querySelector("mch-application-page mch-notification, mch-external-application-page mch-notification"))||void 0===n?void 0:n.outerHTML)&&void 0!==i?i:"")})).catch(console.error),()=>{document.body.style.overflow=t}},[]);return Object(i.jsx)("article",Object.assign({className:"application-overlay__container"},{children:Object(i.jsxs)(d.a,{children:[Object(i.jsxs)("div",Object.assign({className:"application-overlay__content"},{children:[Object(i.jsx)(s.o,Object.assign({level:1,className:"application-overlay__title"},{children:e})),Object(i.jsxs)("div",Object.assign({className:"application-overlay__favorite-toggle"},{children:[Object(i.jsx)("div",Object.assign({className:"application-overlay__toggle-text"},{children:f(x?"removeFromMyApplications":"addToMyApplications")})),Object(i.jsx)(j.a,{icon:x?"favorite-selected":"favorite",onClick:()=>{(t=>{const n={category:"Favorites",parameter:x?"click remove application from favorites":"click add application to favorites",name:e};Object(o.A)("analytics-track-event",{data:n}),w(t)})(y)},flat:!0,iconSize:"large","aria-label":f(x?"removeFromMyApplications":"addToMyApplications")}),Object(i.jsx)(m.a,{onModalClose:()=>null==r?void 0:r("closed"),onModalOpen:()=>null==r?void 0:r("open"),heading:e,iconColor:"light"})]})),Object(i.jsx)(j.a,{icon:"Close",onClick:n,flat:!0,iconSize:"large","aria-label":f("Close")})]})),t&&Object(i.jsx)("p",Object.assign({className:"application-overlay__lead"},{children:t})),b&&Object(i.jsx)("div",Object.assign({className:"mch-notification--application-wrapper"},{children:Object(i.jsx)("div",{dangerouslySetInnerHTML:{__html:b}})})),Object(i.jsx)("div",{dangerouslySetInnerHTML:{__html:l}})]})}))};var g=n(547);n(773);const w=({imageSrc:e,markerProps:t,reportTimestamp:n,title:a})=>Object(i.jsxs)(i.Fragment,{children:[Object(i.jsx)(s.g,{src:e,alt:"",maxHeight:!0,lazy:!0}),Object(i.jsxs)("div",Object.assign({className:"meteo-reports-gallery__overlay-image-legend"},{children:[Object(i.jsx)(g.a,Object.assign({},t)),Object(i.jsxs)("div",Object.assign({className:"meteo-reports-gallery__overlay-image-legend-text"},{children:[Object(i.jsx)(s.o,Object.assign({level:4},{children:a})),n&&Object(i.jsx)(s.m,{html:n})]}))]}))]}),y=()=>{x("canonical","overlay-open-canonical-disabled")},x=(e,t)=>{const n=document.querySelector(`link[rel='${e}']`);n&&(n.rel=""+t)};function k(e){return e.hasAttribute("hidden")||e.hasAttribute("aria-hidden")&&"false"!==e.getAttribute("aria-hidden")||"none"===e.style.display||"0"===e.style.opacity||"hidden"===e.style.visibility||"collapse"===e.style.visibility}function S(e){return"-1"!==e.getAttribute("tabindex")&&!k(e)&&!function(e){return e.hasAttribute("disabled")||e.hasAttribute("aria-disabled")&&"false"!==e.getAttribute("aria-disabled")}(e)&&(e.hasAttribute("tabindex")||(e instanceof HTMLAnchorElement||e instanceof HTMLAreaElement)&&e.hasAttribute("href")||e instanceof HTMLButtonElement||e instanceof HTMLInputElement||e instanceof HTMLTextAreaElement||e instanceof HTMLSelectElement||e instanceof HTMLIFrameElement)}const E=new Map;const _=document.createElement("template");_.innerHTML='\n\t<div id="start"></div>\n\t<div id="backup"></div>\n\t<slot></slot>\n\t<div id="end"></div>\n';class C extends HTMLElement{constructor(){super(),this.debounceId=Math.random().toString(),this._focused=!1;const e=this.attachShadow({mode:"open"});e.appendChild(_.content.cloneNode(!0)),this.$backup=e.querySelector("#backup"),this.$start=e.querySelector("#start"),this.$end=e.querySelector("#end"),this.focusLastElement=this.focusLastElement.bind(this),this.focusFirstElement=this.focusFirstElement.bind(this),this.onFocusIn=this.onFocusIn.bind(this),this.onFocusOut=this.onFocusOut.bind(this)}static get observedAttributes(){return["inactive"]}get inactive(){return this.hasAttribute("inactive")}set inactive(e){e?this.setAttribute("inactive",""):this.removeAttribute("inactive")}get focused(){return this._focused}connectedCallback(){this.$start.addEventListener("focus",this.focusLastElement),this.$end.addEventListener("focus",this.focusFirstElement),this.addEventListener("focusin",this.onFocusIn),this.addEventListener("focusout",this.onFocusOut),this.render()}disconnectedCallback(){this.$start.removeEventListener("focus",this.focusLastElement),this.$end.removeEventListener("focus",this.focusFirstElement),this.removeEventListener("focusin",this.onFocusIn),this.removeEventListener("focusout",this.onFocusOut)}attributeChangedCallback(){this.render()}focusFirstElement(){this.trapFocus()}focusLastElement(){this.trapFocus(!0)}getFocusableElements(){return function e(t,n,i,a=20,s=0){let o=[];if(s>=a)return o;const c=t=>{const o=t.assignedNodes().filter(e=>1===e.nodeType);return o.length>0?e(o[0].parentElement,n,i,a,s+1):[]},r=Array.from(t.children||[]);for(const t of r)n(t)||(i(t)&&o.push(t),null!=t.shadowRoot?o.push(...e(t.shadowRoot,n,i,a,s+1)):"SLOT"===t.tagName?o.push(...c(t)):o.push(...e(t,n,i,a,s+1)));return o}(this,k,S)}trapFocus(e){if(this.inactive)return;let t=this.getFocusableElements();t.length>0?(e?t[t.length-1].focus():t[0].focus(),this.$backup.setAttribute("tabindex","-1")):(this.$backup.setAttribute("tabindex","0"),this.$backup.focus())}onFocusIn(){this.updateFocused(!0)}onFocusOut(){this.updateFocused(!1)}updateFocused(e){!function(e,t,n){const i=E.get(n);null!=i&&window.clearTimeout(i),E.set(n,window.setTimeout(()=>{e(),E.delete(n)},t))}(()=>{this.focused!==e&&(this._focused=e,this.render())},0,this.debounceId)}render(){this.$start.setAttribute("tabindex",!this.focused||this.inactive?"-1":"0"),this.$end.setAttribute("tabindex",!this.focused||this.inactive?"-1":"0"),this.focused?this.setAttribute("focused",""):this.removeAttribute("focused")}}window.customElements.define("focus-trap",C);const L=Object(a.forwardRef)(({children:e},t)=>Object(i.jsx)("focus-trap",Object.assign({ref:t},{children:e}))),A=()=>{const e=Object(a.useRef)(),[t,n]=Object(a.useState)(void 0),[r,d]=Object(a.useState)(void 0),[u,h]=Object(a.useState)("closed"),j=Object(a.useCallback)(()=>{if(document.body.style.overflow="",document.documentElement.style.overflow="",e.current&&(e.current.inactive=!0),null==t?void 0:t.data.scrollPosition){const e=t.data.scrollPosition;window.scrollTo({top:e})}n(void 0),d(void 0),x("overlay-open-canonical-disabled","canonical")},[t]),p=Object(a.useCallback)(t=>{n(t),document.body.style.overflow="hidden",document.documentElement.style.overflow="hidden",e.current&&(e.current.inactive=!1,e.current.focusFirstElement())},[]),m=Object(a.useCallback)(()=>{"closed"===u&&("application-overlay"===(null==t?void 0:t.type)&&(history.back(),Object(o.A)("analytics-reset-context",{})),j())},[t,j,u]),f=Object(a.useCallback)(()=>{if(t)(null==r?void 0:r.data.focusableElementsBefore)&&r.data.focusableElementsBefore.forEach(e=>e.removeAttribute("disabled"));else{if(null==r?void 0:r.data.scrollPosition){const e=r.data.scrollPosition;window.scrollTo({top:e})}j()}(null==r?void 0:r.data.keyboardOrigin)&&r.data.keyboardOrigin.focus(),d(void 0)},[t,r,j]);Object(a.useEffect)(()=>{const e=e=>{"Escape"===e.key&&(r?f():m())};return Object(o.v)("keydown",e),()=>Object(o.u)("keydown",e)},[r,m,f]),Object(a.useEffect)(()=>{const n=e=>{const{isOverlay:t,title:n,lead:i,url:a}=e.state||{};t?((({title:e,lead:t,url:n})=>{e&&n&&p({data:{title:e,lead:t,url:n,keepHistory:!0},type:"application-overlay"})})({title:n,lead:i,url:a}),y()):j()},i=n=>{var i;if(null===(i=n.detail)||void 0===i?void 0:i.type){if(Object(o.A)("analytics-reset-context",{}),"on-top-overlay"===n.detail.type)return d(Object.assign(Object.assign({},n.detail),{data:Object.assign(Object.assign({},n.detail.data),{focusableElementsBefore:e.current?e.current.getFocusableElements():[]})})),void(t?e.current&&e.current.getFocusableElements().forEach(e=>e.setAttribute("disabled","")):p(void 0));if(p(n.detail),"application-overlay"===n.detail.type){const e=n.detail;e.data.keepHistory||(history.pushState(Object.assign({isOverlay:!0},e.data),e.data.title,e.data.url),y())}}};return Object(o.v)(o.c,i),window.addEventListener("popstate",n),()=>{Object(o.u)(o.c,i),window.removeEventListener("popstate",n)}},[t,p,j]);const v=Object(a.useCallback)(e=>{if(e&&(null==t?void 0:t.data.handler)){const n=e=>{var n;e.stopPropagation(),null===(n=t.data.handler)||void 0===n||n.call(null,e.detail)};return e.addEventListener("indexChanged",n),e.focus(),()=>{e.removeEventListener("indexChanged",n)}}},[null==t?void 0:t.data.handler]);return Object(i.jsxs)(L,Object.assign({ref:e},{children:["image"===(null==t?void 0:t.type)&&Object(i.jsx)(b,Object.assign({handleClose:m,ref:v,clickEverywhereToClose:!0},{children:Object(i.jsx)(s.g,{src:t.data.src,viewports:c.d,alt:t.data.alt,caption:t.data.caption,isEnlarged:!0,maxHeight:!0})})),"image-slideshow"===(null==t?void 0:t.type)&&Object(i.jsx)(b,Object.assign({handleClose:m,ref:v},{children:Object(i.jsx)(l,{images:t.data.images,startIndex:t.data.index,maxHeight:!0,isEnlarged:!0})})),"application-overlay"===(null==t?void 0:t.type)&&Object(i.jsx)(O,{title:t.data.title,lead:t.data.lead,close:m,url:t.data.url,setDialogState:h}),"on-top-overlay"===(null==r?void 0:r.type)&&Object(i.jsx)(b,Object.assign({clickEverywhereToClose:!0,handleClose:f,extraClassName:"mch-overlay--on-top"},{children:Object(i.jsx)(w,{imageSrc:r.data.imageSrc||"",markerProps:r.data.markerProps,reportTimestamp:r.data.reportTimestamp,title:r.data.title})}))]}))}},510:function(e,t){},545:function(e,t){},546:function(e,t,n){"use strict";n.d(t,"a",(function(){return a}));var i=n(389);const a=Object(i.s)({openModal:{de:"schaue das Bild in Grossansicht an",fr:"voir l'image en grand format",it:"Ingrandire l'immagine",en:"View the image full-screen"},closeModal:{de:"schliesse die Lightbox",fr:"fermer la lightbox",it:"chiudi la lightbox",en:"close the Lightbox"}})},547:function(e,t,n){"use strict";var i=n(397);const a=Object(i.a)("mch-meteo-reports-map-marker");t.a=a},570:function(e,t){},606:function(e,t,n){"use strict";n.d(t,"a",(function(){return j}));var i=n(7),a=n(389),s=n(9),o=n(398),c=n.n(o),r=n(3),l=n(423),d=n(399);const u=Object(a.s)({facebookShare:{de:"Auf Facebook teilen",fr:"Partager sur Facebook",it:"Condividere su Facebook",en:"Share to Facebook"},twitterShare:{de:"Auf Twitter teilen",fr:"Partager sur Twitter",it:"Condividere su Twitter",en:"Share to Twitter"},linkedinShare:{de:"Auf Linkedin teilen",fr:"Partager sur Linkedin",it:"Condividere su Linkedin",en:"Share to Linkedin"},xingShare:{de:"Auf Xing teilen",fr:"Partager sur Xing",it:"Condividere su Xing",en:"Share to Xing"},copyLink:{de:"Link kopieren",fr:"Copier le lien",it:"Copia il collegamento",en:"Copy link"},mailShare:{de:"Per E-Mail teilen",fr:"Partager par E-Mail",it:"Condividi per e-mail",en:"Share per email"},copiedLink:{de:"Link kopiert",fr:"Lien copié",it:"Collegamento copiato",en:"Link copied"},pageShare:{de:"Seite teilen",fr:"Partager la page",it:"Condividi la pagina",en:"Share page"},websiteName:{de:"MeteoSchweiz",fr:"MétéoSuisse",it:"MeteoSvizzera",en:"MeteoSwiss"}});n(570);const b=(e,t)=>n=>{n.preventDefault(),Object(a.A)("analytics-track-event",{data:{category:"Sharing",parameter:"share button clicked",name:t}}),window.open(e,"popup","height=400,width=600,status=yes,toolbar=no,menubar=no,location=no")},h=e=>()=>{Object(a.A)("analytics-track-event",{data:{category:"Sharing",parameter:"share button clicked",name:e}})},j=({heading:e="",onModalClose:t,onModalOpen:n,iconColor:o="dark"})=>{const j=Object(r.useRef)(null),[p,m]=Object(r.useState)("closed"),[f,v]=Object(r.useState)(!1),O=Object(a.B)(),g=Object(r.useRef)(null),w=Object(r.useCallback)(()=>{var e;null===(e=j.current)||void 0===e||e.close(),m("closed"),v(!1),clearTimeout(g.current),setTimeout(()=>{null==t||t()},0)},[t]);return Object(r.useEffect)(()=>{const e=null==j?void 0:j.current,t=t=>{t.target===e&&w()},n=e=>{"Escape"===e.key&&w()};return e.addEventListener("click",t),Object(a.v)("keydown",n),()=>{e.removeEventListener("click",t),Object(a.u)("keydown",n)}},[j,w]),Object(i.jsxs)(i.Fragment,{children:[Object(i.jsx)(s.c,Object.assign({ariaLabel:u("pageShare"),class:c()({"share-dialog-button__share--dark":"dark"===o,"share-dialog-button__share--light":"light"===o}),kind:"clear",onClick:()=>{var t;!!navigator.share&&!O?navigator.share({title:e,url:window.location.toString()}):(null===(t=j.current)||void 0===t||t.showModal(),m("open"),null==n||n())}},{children:Object(i.jsx)(s.f,{size:"l",name:"share",color:o})})),Object(i.jsx)("dialog",Object.assign({className:"share-dialog",ref:j,open:"open"===p},{children:Object(i.jsxs)("div",Object.assign({className:"share-dialog__wrapper"},{children:[Object(i.jsxs)("div",Object.assign({className:"share-dialog__header"},{children:[Object(i.jsx)(s.o,Object.assign({level:4,className:"share-dialog__title"},{children:u("pageShare")})),Object(i.jsx)(d.a,{icon:"Close",className:"share-dialog__close-button",onClick:w,flat:!0,iconSize:"normal",tabIndex:0})]})),Object(i.jsxs)("div",Object.assign({className:"share-dialog__content"},{children:[Object(i.jsxs)(l.a,Object.assign({direction:"horizontal",className:"share-dialog__link-list"},{children:[Object(i.jsx)(s.i,Object.assign({href:"https://www.facebook.com/sharer/sharer.php?u="+window.location.toString(),"aria-label":u("facebookShare"),onClick:b("https://www.facebook.com/sharer/sharer.php?u="+window.location.toString(),"Facebook")},{children:Object(i.jsx)(s.f,{size:"l",slot:"end",name:"facebook"})})),Object(i.jsx)(s.i,Object.assign({href:"https://twitter.com/share?url="+window.location.toString(),"aria-label":u("twitterShare"),onClick:b("https://twitter.com/share?url="+window.location.toString(),"Twitter")},{children:Object(i.jsx)(s.f,{size:"l",slot:"end",name:"twitter"})})),Object(i.jsx)(s.i,Object.assign({href:"https://www.linkedin.com/shareArticle?url="+window.location.toString(),"aria-label":u("linkedinShare"),onClick:b("https://www.linkedin.com/shareArticle?url="+window.location.toString(),"LinkedIn")},{children:Object(i.jsx)(s.f,{slot:"end",size:"l",name:"linkedin"})})),Object(i.jsx)(s.i,Object.assign({href:"https://www.xing.com/spi/shares/new?url="+window.location.toString(),"aria-label":u("xingShare"),onClick:b("https://www.xing.com/spi/shares/new?url="+window.location.toString(),"Xing")},{children:Object(i.jsx)(s.f,{slot:"end",size:"l",name:"xing"})})),Object(i.jsx)(s.i,Object.assign({href:`mailto:?subject=${encodeURIComponent(`${u("websiteName")} - ${e}`)}&body=${window.location.toString()}`,target:"_blank","aria-label":u("mailShare"),onClick:h("Mail")},{children:Object(i.jsx)(s.f,{size:"l",slot:"end",name:"envelope"})}))]})),Object(i.jsx)(s.n,{type:"text",class:"share-dialog__input",value:window.location.toString(),onFocus:e=>{var t,n;return null===(n=null===(t=e.currentTarget)||void 0===t?void 0:t.select)||void 0===n?void 0:n.call(t)},readonly:!0}),Object(i.jsxs)("div",Object.assign({className:"share-dialog__copy"},{children:[Object(i.jsx)(s.c,Object.assign({ariaLabel:u("copyLink"),title:u("copyLink"),onClick:()=>{navigator.clipboard.writeText(window.location.toString()),h("Link kopieren"),clearTimeout(g.current),v(!0),g.current=setTimeout(()=>{v(!1)},3e3)}},{children:u("copyLink")})),f&&Object(i.jsx)(s.b,Object.assign({class:c()("share-dialog__copy-text",{"share-dialog__copy-text--visible":f}),kind:"success",text:u("copiedLink")},{children:Object(i.jsx)(s.f,{size:"s",slot:"end",name:"checkmark"})}))]}))]}))]}))}))]})}},609:function(e,t){},772:function(e,t){},773:function(e,t){}}]);