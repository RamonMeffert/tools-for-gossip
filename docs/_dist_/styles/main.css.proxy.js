// [snowpack] add styles to the page (skip if no document exists)
if (typeof document !== 'undefined') {
  const code = "/*! normalize.css v8.0.1 | MIT License | github.com/necolas/normalize.css */\n/* Document\n   ========================================================================== */\n/**\n * 1. Correct the line height in all browsers.\n * 2. Prevent adjustments of font size after orientation changes in iOS.\n */\nhtml {\n  line-height: 1.15;\n  /* 1 */\n  -webkit-text-size-adjust: 100%;\n  /* 2 */\n}\n\n/* Sections\n   ========================================================================== */\n/**\n * Remove the margin in all browsers.\n */\nbody {\n  margin: 0;\n}\n\n/**\n * Render the `main` element consistently in IE.\n */\nmain {\n  display: block;\n}\n\n/**\n * Correct the font size and margin on `h1` elements within `section` and\n * `article` contexts in Chrome, Firefox, and Safari.\n */\nh1 {\n  font-size: 2em;\n  margin: 0.67em 0;\n}\n\n/* Grouping content\n   ========================================================================== */\n/**\n * 1. Add the correct box sizing in Firefox.\n * 2. Show the overflow in Edge and IE.\n */\nhr {\n  box-sizing: content-box;\n  /* 1 */\n  height: 0;\n  /* 1 */\n  overflow: visible;\n  /* 2 */\n}\n\n/**\n * 1. Correct the inheritance and scaling of font size in all browsers.\n * 2. Correct the odd `em` font sizing in all browsers.\n */\npre {\n  font-family: monospace, monospace;\n  /* 1 */\n  font-size: 1em;\n  /* 2 */\n}\n\n/* Text-level semantics\n   ========================================================================== */\n/**\n * Remove the gray background on active links in IE 10.\n */\na {\n  background-color: transparent;\n}\n\n/**\n * 1. Remove the bottom border in Chrome 57-\n * 2. Add the correct text decoration in Chrome, Edge, IE, Opera, and Safari.\n */\nabbr[title] {\n  border-bottom: none;\n  /* 1 */\n  text-decoration: underline;\n  /* 2 */\n  text-decoration: underline dotted;\n  /* 2 */\n}\n\n/**\n * Add the correct font weight in Chrome, Edge, and Safari.\n */\nb,\nstrong {\n  font-weight: bolder;\n}\n\n/**\n * 1. Correct the inheritance and scaling of font size in all browsers.\n * 2. Correct the odd `em` font sizing in all browsers.\n */\ncode,\nkbd,\nsamp {\n  font-family: monospace, monospace;\n  /* 1 */\n  font-size: 1em;\n  /* 2 */\n}\n\n/**\n * Add the correct font size in all browsers.\n */\nsmall {\n  font-size: 80%;\n}\n\n/**\n * Prevent `sub` and `sup` elements from affecting the line height in\n * all browsers.\n */\nsub,\nsup {\n  font-size: 75%;\n  line-height: 0;\n  position: relative;\n  vertical-align: baseline;\n}\n\nsub {\n  bottom: -0.25em;\n}\n\nsup {\n  top: -0.5em;\n}\n\n/* Embedded content\n   ========================================================================== */\n/**\n * Remove the border on images inside links in IE 10.\n */\nimg {\n  border-style: none;\n}\n\n/* Forms\n   ========================================================================== */\n/**\n * 1. Change the font styles in all browsers.\n * 2. Remove the margin in Firefox and Safari.\n */\nbutton,\ninput,\noptgroup,\nselect,\ntextarea {\n  font-family: inherit;\n  /* 1 */\n  font-size: 100%;\n  /* 1 */\n  line-height: 1.15;\n  /* 1 */\n  margin: 0;\n  /* 2 */\n}\n\n/**\n * Show the overflow in IE.\n * 1. Show the overflow in Edge.\n */\nbutton,\ninput {\n  /* 1 */\n  overflow: visible;\n}\n\n/**\n * Remove the inheritance of text transform in Edge, Firefox, and IE.\n * 1. Remove the inheritance of text transform in Firefox.\n */\nbutton,\nselect {\n  /* 1 */\n  text-transform: none;\n}\n\n/**\n * Correct the inability to style clickable types in iOS and Safari.\n */\nbutton,\n[type=button],\n[type=reset],\n[type=submit] {\n  -webkit-appearance: button;\n}\n\n/**\n * Remove the inner border and padding in Firefox.\n */\nbutton::-moz-focus-inner,\n[type=button]::-moz-focus-inner,\n[type=reset]::-moz-focus-inner,\n[type=submit]::-moz-focus-inner {\n  border-style: none;\n  padding: 0;\n}\n\n/**\n * Restore the focus styles unset by the previous rule.\n */\nbutton:-moz-focusring,\n[type=button]:-moz-focusring,\n[type=reset]:-moz-focusring,\n[type=submit]:-moz-focusring {\n  outline: 1px dotted ButtonText;\n}\n\n/**\n * Correct the padding in Firefox.\n */\nfieldset {\n  padding: 0.35em 0.75em 0.625em;\n}\n\n/**\n * 1. Correct the text wrapping in Edge and IE.\n * 2. Correct the color inheritance from `fieldset` elements in IE.\n * 3. Remove the padding so developers are not caught out when they zero out\n *    `fieldset` elements in all browsers.\n */\nlegend {\n  box-sizing: border-box;\n  /* 1 */\n  color: inherit;\n  /* 2 */\n  display: table;\n  /* 1 */\n  max-width: 100%;\n  /* 1 */\n  padding: 0;\n  /* 3 */\n  white-space: normal;\n  /* 1 */\n}\n\n/**\n * Add the correct vertical alignment in Chrome, Firefox, and Opera.\n */\nprogress {\n  vertical-align: baseline;\n}\n\n/**\n * Remove the default vertical scrollbar in IE 10+.\n */\ntextarea {\n  overflow: auto;\n}\n\n/**\n * 1. Add the correct box sizing in IE 10.\n * 2. Remove the padding in IE 10.\n */\n[type=checkbox],\n[type=radio] {\n  box-sizing: border-box;\n  /* 1 */\n  padding: 0;\n  /* 2 */\n}\n\n/**\n * Correct the cursor style of increment and decrement buttons in Chrome.\n */\n[type=number]::-webkit-inner-spin-button,\n[type=number]::-webkit-outer-spin-button {\n  height: auto;\n}\n\n/**\n * 1. Correct the odd appearance in Chrome and Safari.\n * 2. Correct the outline style in Safari.\n */\n[type=search] {\n  -webkit-appearance: textfield;\n  /* 1 */\n  outline-offset: -2px;\n  /* 2 */\n}\n\n/**\n * Remove the inner padding in Chrome and Safari on macOS.\n */\n[type=search]::-webkit-search-decoration {\n  -webkit-appearance: none;\n}\n\n/**\n * 1. Correct the inability to style clickable types in iOS and Safari.\n * 2. Change font properties to `inherit` in Safari.\n */\n::-webkit-file-upload-button {\n  -webkit-appearance: button;\n  /* 1 */\n  font: inherit;\n  /* 2 */\n}\n\n/* Interactive\n   ========================================================================== */\n/*\n * Add the correct display in Edge, IE 10+, and Firefox.\n */\ndetails {\n  display: block;\n}\n\n/*\n * Add the correct display in all browsers.\n */\nsummary {\n  display: list-item;\n}\n\n/* Misc\n   ========================================================================== */\n/**\n * Add the correct display in IE 10+.\n */\ntemplate {\n  display: none;\n}\n\n/**\n * Add the correct display in IE 10.\n */\n[hidden] {\n  display: none;\n}\n\n/*!\n * Font Awesome Free 5.15.1 by @fontawesome - https://fontawesome.com\n * License - https://fontawesome.com/license/free (Icons: CC BY 4.0, Fonts: SIL OFL 1.1, Code: MIT License)\n */\n.svg-inline--fa, svg:not(:root).svg-inline--fa {\n  overflow: visible;\n}\n\n.svg-inline--fa {\n  display: inline-block;\n  font-size: inherit;\n  height: 1em;\n  vertical-align: -0.125em;\n}\n\n.svg-inline--fa.fa-lg {\n  vertical-align: -0.225em;\n}\n\n.svg-inline--fa.fa-w-1 {\n  width: 0.0625em;\n}\n\n.svg-inline--fa.fa-w-2 {\n  width: 0.125em;\n}\n\n.svg-inline--fa.fa-w-3 {\n  width: 0.1875em;\n}\n\n.svg-inline--fa.fa-w-4 {\n  width: 0.25em;\n}\n\n.svg-inline--fa.fa-w-5 {\n  width: 0.3125em;\n}\n\n.svg-inline--fa.fa-w-6 {\n  width: 0.375em;\n}\n\n.svg-inline--fa.fa-w-7 {\n  width: 0.4375em;\n}\n\n.svg-inline--fa.fa-w-8 {\n  width: 0.5em;\n}\n\n.svg-inline--fa.fa-w-9 {\n  width: 0.5625em;\n}\n\n.svg-inline--fa.fa-w-10 {\n  width: 0.625em;\n}\n\n.svg-inline--fa.fa-w-11 {\n  width: 0.6875em;\n}\n\n.svg-inline--fa.fa-w-12 {\n  width: 0.75em;\n}\n\n.svg-inline--fa.fa-w-13 {\n  width: 0.8125em;\n}\n\n.svg-inline--fa.fa-w-14 {\n  width: 0.875em;\n}\n\n.svg-inline--fa.fa-w-15 {\n  width: 0.9375em;\n}\n\n.svg-inline--fa.fa-w-16 {\n  width: 1em;\n}\n\n.svg-inline--fa.fa-w-17 {\n  width: 1.0625em;\n}\n\n.svg-inline--fa.fa-w-18 {\n  width: 1.125em;\n}\n\n.svg-inline--fa.fa-w-19 {\n  width: 1.1875em;\n}\n\n.svg-inline--fa.fa-w-20 {\n  width: 1.25em;\n}\n\n.svg-inline--fa.fa-pull-left {\n  margin-right: 0.3em;\n  width: auto;\n}\n\n.svg-inline--fa.fa-pull-right {\n  margin-left: 0.3em;\n  width: auto;\n}\n\n.svg-inline--fa.fa-border {\n  height: 1.5em;\n}\n\n.svg-inline--fa.fa-li {\n  width: 2em;\n}\n\n.svg-inline--fa.fa-fw {\n  width: 1.25em;\n}\n\n.fa-layers svg.svg-inline--fa {\n  bottom: 0;\n  left: 0;\n  margin: auto;\n  position: absolute;\n  right: 0;\n  top: 0;\n}\n\n.fa-layers {\n  display: inline-block;\n  height: 1em;\n  position: relative;\n  text-align: center;\n  vertical-align: -0.125em;\n  width: 1em;\n}\n\n.fa-layers svg.svg-inline--fa {\n  -webkit-transform-origin: center center;\n  transform-origin: center center;\n}\n\n.fa-layers-counter, .fa-layers-text {\n  display: inline-block;\n  position: absolute;\n  text-align: center;\n}\n\n.fa-layers-text {\n  left: 50%;\n  top: 50%;\n  -webkit-transform: translate(-50%, -50%);\n  transform: translate(-50%, -50%);\n  -webkit-transform-origin: center center;\n  transform-origin: center center;\n}\n\n.fa-layers-counter {\n  background-color: #ff253a;\n  border-radius: 1em;\n  -webkit-box-sizing: border-box;\n  box-sizing: border-box;\n  color: #fff;\n  height: 1.5em;\n  line-height: 1;\n  max-width: 5em;\n  min-width: 1.5em;\n  overflow: hidden;\n  padding: 0.25em;\n  right: 0;\n  text-overflow: ellipsis;\n  top: 0;\n  -webkit-transform: scale(0.25);\n  transform: scale(0.25);\n  -webkit-transform-origin: top right;\n  transform-origin: top right;\n}\n\n.fa-layers-bottom-right {\n  bottom: 0;\n  right: 0;\n  top: auto;\n  -webkit-transform: scale(0.25);\n  transform: scale(0.25);\n  -webkit-transform-origin: bottom right;\n  transform-origin: bottom right;\n}\n\n.fa-layers-bottom-left {\n  bottom: 0;\n  left: 0;\n  right: auto;\n  top: auto;\n  -webkit-transform: scale(0.25);\n  transform: scale(0.25);\n  -webkit-transform-origin: bottom left;\n  transform-origin: bottom left;\n}\n\n.fa-layers-top-right {\n  right: 0;\n  top: 0;\n  -webkit-transform: scale(0.25);\n  transform: scale(0.25);\n  -webkit-transform-origin: top right;\n  transform-origin: top right;\n}\n\n.fa-layers-top-left {\n  left: 0;\n  right: auto;\n  top: 0;\n  -webkit-transform: scale(0.25);\n  transform: scale(0.25);\n  -webkit-transform-origin: top left;\n  transform-origin: top left;\n}\n\n.fa-lg {\n  font-size: 1.33333em;\n  line-height: 0.75em;\n  vertical-align: -0.0667em;\n}\n\n.fa-xs {\n  font-size: 0.75em;\n}\n\n.fa-sm {\n  font-size: 0.875em;\n}\n\n.fa-1x {\n  font-size: 1em;\n}\n\n.fa-2x {\n  font-size: 2em;\n}\n\n.fa-3x {\n  font-size: 3em;\n}\n\n.fa-4x {\n  font-size: 4em;\n}\n\n.fa-5x {\n  font-size: 5em;\n}\n\n.fa-6x {\n  font-size: 6em;\n}\n\n.fa-7x {\n  font-size: 7em;\n}\n\n.fa-8x {\n  font-size: 8em;\n}\n\n.fa-9x {\n  font-size: 9em;\n}\n\n.fa-10x {\n  font-size: 10em;\n}\n\n.fa-fw {\n  text-align: center;\n  width: 1.25em;\n}\n\n.fa-ul {\n  list-style-type: none;\n  margin-left: 2.5em;\n  padding-left: 0;\n}\n\n.fa-ul > li {\n  position: relative;\n}\n\n.fa-li {\n  left: -2em;\n  position: absolute;\n  text-align: center;\n  width: 2em;\n  line-height: inherit;\n}\n\n.fa-border {\n  border: 0.08em solid #eee;\n  border-radius: 0.1em;\n  padding: 0.2em 0.25em 0.15em;\n}\n\n.fa-pull-left {\n  float: left;\n}\n\n.fa-pull-right {\n  float: right;\n}\n\n.fa.fa-pull-left, .fab.fa-pull-left, .fal.fa-pull-left, .far.fa-pull-left, .fas.fa-pull-left {\n  margin-right: 0.3em;\n}\n\n.fa.fa-pull-right, .fab.fa-pull-right, .fal.fa-pull-right, .far.fa-pull-right, .fas.fa-pull-right {\n  margin-left: 0.3em;\n}\n\n.fa-spin {\n  -webkit-animation: fa-spin 2s linear infinite;\n  animation: fa-spin 2s linear infinite;\n}\n\n.fa-pulse {\n  -webkit-animation: fa-spin 1s steps(8) infinite;\n  animation: fa-spin 1s steps(8) infinite;\n}\n\n@-webkit-keyframes fa-spin {\n  0% {\n    -webkit-transform: rotate(0deg);\n    transform: rotate(0deg);\n  }\n  to {\n    -webkit-transform: rotate(1turn);\n    transform: rotate(1turn);\n  }\n}\n@keyframes fa-spin {\n  0% {\n    -webkit-transform: rotate(0deg);\n    transform: rotate(0deg);\n  }\n  to {\n    -webkit-transform: rotate(1turn);\n    transform: rotate(1turn);\n  }\n}\n.fa-rotate-90 {\n  -ms-filter: \"progid:DXImageTransform.Microsoft.BasicImage(rotation=1)\";\n  -webkit-transform: rotate(90deg);\n  transform: rotate(90deg);\n}\n\n.fa-rotate-180 {\n  -ms-filter: \"progid:DXImageTransform.Microsoft.BasicImage(rotation=2)\";\n  -webkit-transform: rotate(180deg);\n  transform: rotate(180deg);\n}\n\n.fa-rotate-270 {\n  -ms-filter: \"progid:DXImageTransform.Microsoft.BasicImage(rotation=3)\";\n  -webkit-transform: rotate(270deg);\n  transform: rotate(270deg);\n}\n\n.fa-flip-horizontal {\n  -ms-filter: \"progid:DXImageTransform.Microsoft.BasicImage(rotation=0, mirror=1)\";\n  -webkit-transform: scaleX(-1);\n  transform: scaleX(-1);\n}\n\n.fa-flip-vertical {\n  -webkit-transform: scaleY(-1);\n  transform: scaleY(-1);\n}\n\n.fa-flip-both, .fa-flip-horizontal.fa-flip-vertical, .fa-flip-vertical {\n  -ms-filter: \"progid:DXImageTransform.Microsoft.BasicImage(rotation=2, mirror=1)\";\n}\n\n.fa-flip-both, .fa-flip-horizontal.fa-flip-vertical {\n  -webkit-transform: scale(-1);\n  transform: scale(-1);\n}\n\n:root .fa-flip-both, :root .fa-flip-horizontal, :root .fa-flip-vertical, :root .fa-rotate-90, :root .fa-rotate-180, :root .fa-rotate-270 {\n  -webkit-filter: none;\n  filter: none;\n}\n\n.fa-stack {\n  display: inline-block;\n  height: 2em;\n  position: relative;\n  width: 2.5em;\n}\n\n.fa-stack-1x, .fa-stack-2x {\n  bottom: 0;\n  left: 0;\n  margin: auto;\n  position: absolute;\n  right: 0;\n  top: 0;\n}\n\n.svg-inline--fa.fa-stack-1x {\n  height: 1em;\n  width: 1.25em;\n}\n\n.svg-inline--fa.fa-stack-2x {\n  height: 2em;\n  width: 2.5em;\n}\n\n.fa-inverse {\n  color: #fff;\n}\n\n.sr-only {\n  border: 0;\n  clip: rect(0, 0, 0, 0);\n  height: 1px;\n  margin: -1px;\n  overflow: hidden;\n  padding: 0;\n  position: absolute;\n  width: 1px;\n}\n\n.sr-only-focusable:active, .sr-only-focusable:focus {\n  clip: auto;\n  height: auto;\n  margin: 0;\n  overflow: visible;\n  position: static;\n  width: auto;\n}\n\n.svg-inline--fa .fa-primary {\n  fill: var(--fa-primary-color, currentColor);\n  opacity: 1;\n  opacity: var(--fa-primary-opacity, 1);\n}\n\n.svg-inline--fa .fa-secondary {\n  fill: var(--fa-secondary-color, currentColor);\n}\n\n.svg-inline--fa .fa-secondary, .svg-inline--fa.fa-swap-opacity .fa-primary {\n  opacity: 0.4;\n  opacity: var(--fa-secondary-opacity, 0.4);\n}\n\n.svg-inline--fa.fa-swap-opacity .fa-secondary {\n  opacity: 1;\n  opacity: var(--fa-primary-opacity, 1);\n}\n\n.svg-inline--fa mask .fa-primary, .svg-inline--fa mask .fa-secondary {\n  fill: #000;\n}\n\n.fad.fa-inverse {\n  color: #fff;\n}\n\nbody {\n  font-size: 18px;\n  font-family: -apple-system, system-ui, BlinkMacSystemFont, \"Segoe UI\", Helvetica, Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";\n  line-height: 27px;\n}\n\nh1 {\n  font-size: 41.6350937812px;\n  font-family: -apple-system, system-ui, BlinkMacSystemFont, \"Segoe UI\", Helvetica, Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";\n  font-weight: bold;\n  margin-top: 0;\n  margin-bottom: 20.8175468906px;\n}\n\nh1 + p.subtitle {\n  font-size: 18.1022146875px;\n}\n\nh2 {\n  font-size: 36.204429375px;\n  font-family: -apple-system, system-ui, BlinkMacSystemFont, \"Segoe UI\", Helvetica, Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";\n  font-weight: bold;\n  margin-top: 0;\n  margin-bottom: 18.1022146875px;\n}\n\nh2 + p.subtitle {\n  font-size: 15.74105625px;\n}\n\nh3 {\n  font-size: 31.4821125px;\n  font-family: -apple-system, system-ui, BlinkMacSystemFont, \"Segoe UI\", Helvetica, Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";\n  font-weight: bold;\n  margin-top: 0;\n  margin-bottom: 15.74105625px;\n}\n\nh3 + p.subtitle {\n  font-size: 13.687875px;\n}\n\nh4 {\n  font-size: 27.37575px;\n  font-family: -apple-system, system-ui, BlinkMacSystemFont, \"Segoe UI\", Helvetica, Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";\n  font-weight: bold;\n  margin-top: 0;\n  margin-bottom: 13.687875px;\n}\n\nh4 + p.subtitle {\n  font-size: 11.9025px;\n}\n\nh5 {\n  font-size: 23.805px;\n  font-family: -apple-system, system-ui, BlinkMacSystemFont, \"Segoe UI\", Helvetica, Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";\n  font-weight: bold;\n  margin-top: 0;\n  margin-bottom: 11.9025px;\n}\n\nh5 + p.subtitle {\n  font-size: 10.35px;\n}\n\nh6 {\n  font-size: 20.7px;\n  font-family: -apple-system, system-ui, BlinkMacSystemFont, \"Segoe UI\", Helvetica, Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";\n  font-weight: bold;\n  margin-top: 0;\n  margin-bottom: 10.35px;\n}\n\nh6 + p.subtitle {\n  font-size: 9px;\n}\n\n.call-list {\n  display: flex;\n  flex-wrap: wrap;\n}\n.call-list .call {\n  padding: 0.325rem 0.75rem;\n  background-color: #c7dae9;\n  margin: 0.325rem;\n  border-radius: 0.3rem;\n  text-align: center;\n  border: 2px solid rgba(0, 0, 0, 0.1);\n}\n\ninput, button, select {\n  -webkit-appearance: none;\n  -moz-appearance: none;\n  appearance: none;\n  box-sizing: border-box;\n  padding: 0 1.5rem;\n  line-height: 3rem;\n  border: 2px solid #bfbfbf;\n}\ninput:focus, button:focus, select:focus {\n  outline: none;\n  box-shadow: 0 0 0 4px steelblue;\n  z-index: 5;\n}\n\nbutton,\ninput[type=button] {\n  appearance: none;\n  background-color: #e6e6e6;\n}\nbutton:active,\ninput[type=button]:active {\n  background-color: #cccccc;\n}\n\nselect {\n  background-image: url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 48'><path d='M23.7 17.3c.9 1.5-.2 3.5-2 3.5H2.3c-1.8 0-2.9-1.9-2-3.5L10 1.1c.9-1.5 3-1.5 3.9 0 .1 0 9.8 16.2 9.8 16.2zM.3 30.7c-.9-1.5.2-3.5 2-3.5h19.4c1.8 0 2.9 1.9 2 3.5L14 46.9c-.9 1.5-3 1.5-3.9 0C10 46.9.3 30.7.3 30.7z'/></svg>\");\n  background-repeat: no-repeat;\n  background-position: center right 1rem;\n  background-origin: padding-box;\n  background-size: 0.5rem;\n  padding-right: 2.25rem;\n}\n\n.input-group {\n  display: flex;\n  align-items: flex-start;\n  width: 100%;\n}\n.input-group:not(:last-child) {\n  margin-bottom: 1.3rem;\n}\n.input-group input:not([type=button]), .input-group select {\n  width: 100%;\n}\n.input-group input, .input-group button, .input-group select {\n  border-right-width: 0;\n  flex-grow: 1;\n}\n.input-group input:first-child, .input-group button:first-child, .input-group select:first-child {\n  border-radius: 0.3rem 0 0 0.3rem;\n}\n.input-group input:last-child, .input-group button:last-child, .input-group select:last-child {\n  border-right-width: 2px;\n  border-radius: 0 0.3rem 0.3rem 0;\n}\n.input-group input:only-child, .input-group button:only-child, .input-group select:only-child {\n  border-radius: 0.3rem;\n}\n.input-group:focus-within {\n  box-shadow: 0 0 0 4px rgba(70, 130, 180, 0.5);\n  border-radius: 0.3rem;\n}\n\n.alert {\n  border-radius: 0.3rem;\n  padding: 1.3rem 1.5rem;\n  border: 2px solid rgba(0, 0, 0, 0.1);\n  display: flex;\n}\n.alert .icon {\n  padding-inline-end: 1.5rem;\n  display: flex;\n  align-items: center;\n  color: rgba(0, 0, 0, 0.7);\n}\n.alert .content {\n  display: flex;\n  align-items: center;\n}\n.alert.info {\n  background-color: #c7dae9;\n}\n.alert.warning {\n  background-color: #ffefd6;\n}\n.alert.error {\n  background-color: #f1c6d4;\n}\n\nheader {\n  margin-bottom: 1.3rem;\n}\n\nmain {\n  width: 1000px;\n  margin: 2rem auto 0 auto;\n  padding: 1.3rem 1.5rem;\n  background-color: white;\n}\nmain section {\n  border-radius: 0.3rem;\n  padding: 1.3rem 1.5rem;\n  border: 2px solid rgba(0, 0, 0, 0.1);\n  margin-bottom: 1.3rem;\n}\n\n#gossip-graph {\n  width: 100%;\n  box-sizing: border-box;\n  position: relative;\n}\n\n#gossip-graph.empty {\n  padding: 5.2rem 1.5rem;\n  display: flex;\n  flex-direction: column;\n  align-items: center;\n  color: #b3b3b3;\n}\n#gossip-graph.empty svg {\n  color: #e6e6e6;\n  margin-bottom: 2.6rem;\n}\n\n#graph-history,\n#graph-input-examples,\n#canonical-representation {\n  border-radius: 0.3rem;\n  padding: 1.3rem 1.5rem;\n  border: 2px solid rgba(0, 0, 0, 0.1);\n  margin-bottom: 1.3rem;\n}\n\nhr {\n  border: 2px solid #bfbfbf;\n  width: 75%;\n  margin-left: 0;\n}\n\n.columns {\n  columns: 2;\n}\n.columns div {\n  -webkit-column-break-inside: avoid;\n  page-break-inside: avoid;\n  break-inside: avoid;\n}\n\n.connection-info-container {\n  position: absolute;\n  top: 0;\n  left: 0;\n  display: flex;\n  flex-direction: column;\n  align-items: flex-start;\n}\n\n.connection-info {\n  border-radius: 0.3rem;\n  padding: 1.3rem 1.5rem;\n  border: 2px solid rgba(0, 0, 0, 0.1);\n  padding: 0.65rem 0.75rem;\n  display: flex;\n  align-content: center;\n  transition: all 0.2s;\n  margin-bottom: 0.65rem;\n}\n.connection-info .explanation {\n  display: none;\n}\n.connection-info .divider {\n  width: 2px;\n  background-color: rgba(0, 0, 0, 0.1);\n  margin-right: 0.75rem;\n}\n.connection-info > div:not(:last-child) .icon {\n  margin-right: 0.75rem;\n}\n.connection-info > div .icon {\n  opacity: 0.1;\n  width: 1em;\n  display: block;\n  text-align: center;\n}\n.connection-info > div.visible .icon {\n  opacity: 1;\n}\n.connection-info:hover {\n  flex-direction: column;\n}\n.connection-info:hover > div {\n  display: flex;\n}\n.connection-info:hover > div .icon {\n  margin-right: 0.75rem;\n}\n.connection-info:hover .explanation {\n  display: block;\n}\n.connection-info:hover .divider {\n  width: 100%;\n  height: 2px;\n  margin: 0.65rem 0;\n}";

  const styleEl = document.createElement("style");
  const codeEl = document.createTextNode(code);
  styleEl.type = 'text/css';

  styleEl.appendChild(codeEl);
  document.head.appendChild(styleEl);
}