module SpaceMan.Encoding where

import SpaceMan.Alphabet

-- Instruction Manipulation Prefixes
stack       = [ space              ]
arithmetic  = [ tabular, space     ]
heap        = [ tabular, tabular   ]
flowControl = [ linefeed           ]
io          = [ tabular, linefeed  ]

-- Stack Encoding
push        = [ space              ]
duplicate   = [ linefeed, space    ]
swap        = [ linefeed, tabular  ]
drop        = [ linefeed, linefeed ]
copy        = [ tabular,  space    ]
slide       = [ tabular,  linefeed ]

-- Arithmetic Encoding
add         = [ space,   space     ]
subtract    = [ space,   tabular   ]
multiply    = [ space,   linefeed  ]
divide      = [ tabular, space     ]
modulo      = [ tabular, tabular   ]

-- Heap Encoding
store       = [ space              ]
fetch       = [ tabular            ]

-- Flow Control Encoding
tag         = [ space,    space    ]
call        = [ space,    tabular  ]
jump        = [ space,    linefeed ]
jumpIfZero  = [ tabular,  space    ]
jumpIfNeg   = [ tabular,  tabular  ]
callReturn  = [ tabular,  linefeed ]
exit        = [ linefeed, linefeed ]

-- Input/Output Encoding
printChar   = [ space,   space     ]
printNum    = [ space,   tabular   ]
readChar    = [ tabular, space     ]
readNum     = [ tabular, tabular   ]
