module Widgets.Page.TwoWindow.Style (css) where

import Utils.Style

css :: String
css =
  concat
    [ imp "https://fonts.googleapis.com/css2?family=Poppins&family=Roboto+Mono&family=Slabo+27px&display=swap",
      sel
        ":root"
        [ ppt "--bg-col" "#2c2828",
          ppt "--fg-col" "#8f938f",
          ppt "--fg2-col" "#545558"
        ],
      sel
        "h1"
        [ ppt "font-family" "'Slabo 27px', serif",
          ppt "font-size" "3rem"
        ],
      sel
        "html"
        [ ppt "height" "100%",
          ppt "background-color" "var(--bg-col)",
          ppt "color" "var(--fg-col)",
          ppt "font-size" "12px"
        ],
      sel
        "body"
        [ ppt "padding" "0",
          ppt "margin" "0",
          ppt "height" "100%",
          ppt "display" "flex"
        ],
      sel
        ".container"
        [ ppt "display" "flex",
          ppt "flex-direction" "column",
          ppt "width" "100%",
          ppt "flex" "1 1 auto",
          ppt "margin" "1rem"
        ],
      sel
        ".content"
        [ ppt "display" "grid",
          ppt "grid-template-columns" "50% 50%",
          ppt "flex" "1 1 auto",
          ppt "overflow" "scroll"
        ],
      sel
        ".tabwindow"
        [ ppt "display" "flex",
          ppt "flex-direction" "column",
          ppt "overflow" "scroll"
        ],
      sel
        ".tablist"
        [ ppt "display" "flex",
          ppt "flex-wrap" "nowrap",
          ppt "justify-content" "flex-start",
          ppt "gap" "1rem"
        ],
      sel
        ".tabbtn"
        [ ppt "user-select" "none",
          ppt "cursor" "pointer",
          ppt "font-family" "'Roboto Mono', monospace",
          ppt "letter-spacing" "0.1rem",
          ppt "color" "var(--fg2-col)",
          ppt "background-color" "var(--bg-col)",
          ppt "border-bottom" "0.2rem solid var(--fg2-col)"
        ],
      sel
        ".tabselected"
        [ ppt "color" "var(--fg-col)",
          ppt "border-bottom" "0.2rem solid var(--fg-col)"
        ],
      sel
        ".section"
        [ ppt "font-family" "'Poppins', sans-serif"
        ],
      sel
        "textarea"
        [ ppt "resize" "none",
          ppt "flex-grow" "1",
          ppt "height" "100%",
          ppt "font-family" "'Roboto Mono', monospace",
          ppt "border" "none",
          ppt "background-color" "inherit",
          ppt "color" "inherit",
          ppt "padding-left" "1rem",
          ppt "outline" "none"
        ],
      sel
        "::selection"
        [ ppt "background-color" "var(--fg2-col)"
        ],
      sel
        ".d3div"
        [ ppt "overflow" "scroll",
          ppt "width" "100%",
          ppt "height" "100%"
        ],
      sel
        "@media only screen and (max-width: 600px)"
        [ sel
            ".content"
            [ ppt "grid-template-columns" "1fr",
              ppt "grid-template-rows" "1fr 1fr"
            ]
        ]
    ]