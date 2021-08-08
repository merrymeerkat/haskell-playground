{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

import MapX

$(genTupleMapXBoilerplate 62)
-- $(genTupleMapXClass 2)
-- $(genTupleMapXInstance 2 3)
-- $(genTupleMapXClass 4)
-- $(genTupleMapXInstance 3 4) 
-- $(genTupleMapBoilerplate)
--
-- Note: the largest tuple size GHC can handle is 62
