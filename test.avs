LoadCPlugin("z:\devel\haskell\hasmerge\bin\hasmerge.dll")

base=AviSource("z:\402B.avi").trim(100,399)
MessageClip(string(base.framerate))
/*MergeClips(base,inverted,"z:\devel\haskell\hasmerge\merge1.txt")*/
/*StackVertical(base,res)*/

/*Invert()*/
