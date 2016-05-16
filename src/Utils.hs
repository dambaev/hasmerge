module Utils where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr


split:: (a-> Bool) -> [a]-> [[a]]
split foo lls = split' foo lls []
    where
    split' foo [] tmp = tmp
    split' foo ls tmp = 
        let dropped = dropWhile foo ls
            val = takeWhile (not . foo) dropped
            newls = dropWhile foo $ drop (length val + length (takeWhile foo ls)) ls
            newtmp = tmp ++ [val]
        in split' foo newls newtmp

--fromPtr:: Ptr a-> IO (ForeignPtr a)
--fromPtr ptr = newForeignPtr finalizerFree

