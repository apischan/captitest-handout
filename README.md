Representative output was produced by processor Intel(R) Core(TM) i5-3210M CPU @ 2.50GHz
which have 2 cores and 4 threads (taken from /proc/cpuinfo file and spec in internet). 

Representative output:
======================
<pre><code>
sampled merged iterator after 1000000 in 304 millis:
960790
960792
960793
960793
960794
960794
960796
960796
960799
960799
960800
approximate sparsities in 1652 millis by 1000000 elems with 4 cores:
(2,Success(0.499999750000125))
(3,Success(0.33333300000033333))
(4,Success(0.2500263777828561))
(5,Success(0.20185533310873563))
(6,Success(0.17515797497763233))
(7,Success(0.12411635362039956))
(8,Success(0.08479267893226684))
sampled merged iterator after 10000000 in 1787 millis:
9682357
9682358
9682358
9682359
9682360
9682361
9682361
9682362
9682362
9682363
9682364
approximate sparsities in 466619 millis by 10000000 elems with 4 cores:
(0,Failure(java.lang.ArithmeticException: / by zero))
(1,Failure(java.lang.IllegalArgumentException: requirement failed: sparsity of 1 is not supported))
(2,Success(0.5))
(3,Success(0.33333338888889813))
(4,Success(0.2500922089974574))
(5,Success(0.1980380254000799))
(6,Success(0.17317002061208123))
(7,Success(0.15491799570815185))
(8,Success(0.1032671591525678))
(9,Success(0.06998620718827354))
(10,Success(0.07143621459327423))
(11,Success(0.11628740324728154))
(12,Success(0.14467952407554813))
(13,Success(0.15757439059404427))
(14,Success(0.14204988351554426))
(15,Success(0.11501208328448195))
(16,Success(0.08757484354119284))
(17,Success(0.060963774977414904))
(18,Success(0.04185682273556462))
(19,Success(0.026130355543100784))
(20,Success(0.018629405986784656))
(21,Success(0.010291265569608084))
(22,Success(0.0062133264811940455))
(23,Success(0.003446775528223261))
(24,Success(0.002332971054579198))
</code></pre>