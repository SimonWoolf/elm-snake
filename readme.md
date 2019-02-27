# Snake

Try it at http://simonwoolf.net/elm-snake/ :)

Nothing that hasn't been done before, just written to help teach myself Elm.

NB: to recompile, you'll need to apply a couple of fixes to elm-graphics: https://github.com/evancz/elm-graphics/pull/21 and https://github.com/evancz/elm-graphics/pull/20

### License

GPLv2+

### TODO
- allow js input through ports


x joins, broadcasts 'I am here', starts counting local clock ticks from 0
Y etc reply 'my local clock at the time I got your tt0 message is 100'
x stores that, starts counting up
when y does a move it broadcasts it along with its current clock, say 250.
x , which has its view of ys clock as 260, rewinds ys position 10, plays the move, and fast forwards 10. if its view of y's clock is only 240, it subtracts 10 from its current stored offset of y, and applies the move immediately (so the stored offset is effectively the minimum latency).

this way, the move is made with everyone else's snakes in the positions they were at the time y made the move

problem: Time.every doesn't tick for background tabs.
Solution: use a 'little tick'; every time set number of big ticks since last as floor((now - start)/littleTicksPerBigTick).
elm - check how Time.every works when background tab in chrome

desirable properties:
-fair - player should only die if they had an opportunity to anticipate the crash. means crashes calculated by the crasher. 
