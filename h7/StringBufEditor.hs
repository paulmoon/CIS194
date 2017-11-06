module Main where

import Buffer
import Editor
import JoinList
import Scrabble
import Sized
import StringBuffer

initBuffer :: JoinList (Score, Size) String
initBuffer = fromString $ unlines 
         [ 
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque nec est a"
        , "turpis eleifend sodales. Morbi vulputate nisl orci, vel sollicitudin arcu"
        , "tincidunt sit amet. Phasellus vel quam at augue bibendum bibendum. Nunc sit"
        , "amet augue erat. Proin magna metus, aliquet at metus ac, posuere bibendum"
        , "massa. Vestibulum nibh ante, maximus a magna sit amet, rutrum vestibulum ex."
        , "Sed quis dolor quis purus dignissim consectetur. Vestibulum mattis convallis"
        , "convallis. Praesent orci ante, blandit ac aliquet et, bibendum quis sapien."
        , "Praesent tempor enim nec tortor efficitur, sed rhoncus nulla tristique."

        , "Integer et dictum nisi, at ultricies nisl. Maecenas consequat nisi ultrices,"
        , "maximus est quis, malesuada ligula. Suspendisse pellentesque sapien purus, non"
        , "fermentum arcu tincidunt sed. Sed rhoncus velit at odio ultricies tempor."
        , "Vivamus velit nisl, placerat nec mi ac, pellentesque vulputate turpis. Sed in"
        , "augue nec justo venenatis dapibus. Aliquam egestas in lectus vel iaculis."
        , "Mauris consectetur sapien eros. In laoreet erat vel felis pretium, ut feugiat"
        , "leo congue. Donec sed placerat eros. Curabitur porta efficitur mauris, vel"
        , "cursus mauris cursus in. Mauris accumsan mattis tincidunt."

        , "Cras velit erat, vulputate eu rhoncus et, cursus et risus. Fusce varius turpis"
        , "augue, id iaculis enim auctor auctor. Nulla nec tellus ut purus tempor euismod."
        , "Morbi in euismod quam. Duis laoreet non massa sodales maximus. Nunc blandit"
        , "ullamcorper mauris, quis condimentum dui lacinia non. Morbi eleifend dolor"
        , "enim, non interdum tortor semper tempor. Proin orci eros, rutrum eu tellus"
        , "vitae, sodales pretium risus. Pellentesque pretium est sit amet euismod"
        , "interdum. Vivamus nec diam orci."
         ]

main :: IO()
main = runEditor editor $ initBuffer
