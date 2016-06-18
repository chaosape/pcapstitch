This is an early release of PcapStitch and this README is inadequate.



--BUILDING--
* Download the GHC Platform: http://hackage.haskell.org/platform/contents.html
* Install dependencies (NOTE: these have not been ordered, you may have to do 
  a bit of trial and error to get everything installed):
** cabal install network
** cabal install pcap
** cabal install pathtype
** cabal install bytestring
** cabal install bytestring-nums
** cabal install 'QuickCheck < 2'
** cabal install containers
** cabal install records
** cabal install binary-strict
** cabal install type-functions
** cabal install kinds
** cabal install template-haskell
** cabal install binary
* Perform the following steps in the pcapstitch direction (NOTE: these have to 
  be done because I have this package compiling with profiling and currently it 
  seems that cabal does not handle template-haskell and profiling elegantly):
** ghc -c -i./src src/Network/PcapStitch/HeaderNinja.hs
** ghc -c -O2 -ddump-splices -prof -auto-all -osuf p_o -i./src src/Network/PcapStitch/HeaderNinja.hs
** cp src/Network/PcapStitch/HeaderNinja.p_o dist/build/pcapstitch/pcapstitch-tmp/Network/PcapStitch/HeaderNinja.p_o
** cp src/Network/PcapStitch/HeaderNinja.o dist/build/pcapstitch/pcapstitch-tmp/Network/PcapStitch/HeaderNinja.o
* cabal configure
* cabal build

--RUNNING--
* Binary file should now exist in ./dist/build/pcapstitch/pcapstitch
* To run "./dist/build/pcapstitch/pcapstitch [list of pcap files]"
* If you provide a -h you will receive mildly useful information
* Further questions can be referred to chaosape@chaosape.com

--OUTPUT--
PcapStitch outputs results in a space delimited text file.
The output columns are described as:
* Flow ID - This is a unique integer associated with a quintuple containing network source address, network destination address, transport protocol, transport source address and
transport destination address.  This ID will be associate with traffic bi-directionally by defining the quintuple as (src_l,dst_l,proto,src_t,dest_t) where:
** src_l is the greater of the source and destination network address when compared in integral form
** dst_l is the lesser of the source and destination network address when compared in integral form
** proto is the transport layer protocol
** src_t is the transport source address if the network source address is greater than the network destination address and the transport destination address otherwise
** dst_t is the transport destination address if the network source address is greater than the network destination address and the transport source address otherwise
* Wire length - This is size in byte of the packet at the link layer.
* Link Layer Type - The protocol used at the link layer.
* Link layer Source Address - The link layer source address.
* Link Layer Destination Address - The link layer destination address.
* Network Layer Type - The protocol used at the network layer.
* Network Layer Source Address - The network layer source address.
* Network Layer Destination Address - The network layer destination address.
* Transport Layer Type - The protocol used at the transport layer.
* Transport Layer Source Address - The transport layer source address.
* Transport Layer Destination Address - The transport layer destination address.
* Merge Records - Each merge is separated by a \emph{|}. Each merge record is a triple containing time of packet recording, network trace file that the packet was recorded in, and offset of the packet within that network trace file (starting at 1). The merge records are chronologically order from left to right.
This text format makes processing and analyzing with a Unix environment straight forward.

--PROCESSING OUTPUT EXAMPLE--
#!/bin/bash
cat $1 |\
sed -e "s/|\|,/ /g" -e "s/(\|)//g" |\
awk 'BEGIN{init=0;print "plot \"-\" using 1:2 with lines";}{
  if(init==0){init=$12;}
  if(NF>14){printf("\t%f %f\n",$12-init,$15-$12);}
}END{print "end";}' |\
gnuplot  -persist


--PERFORMANCE--
* Here are some results from this build where baseline is tcpdump outputing
  to /dev/null :
#baseline/pcapstitch(0/1) filesize(MB) exec_time(secs) packet_horizon(us)
0 0.5 1.164 0
1 0.5 1.767 0.001
1 0.5 1.737 0.005
1 0.5 1.725 0.01
1 0.5 1.633 0.05
1 0.5 1.601 0.1
1 0.5 1.694 0.5
1 0.5 1.624 1.0
0 1 0.246 0
1 1 3.374 0.001
1 1 3.066 0.005
1 1 3.186 0.01
1 1 3.307 0.05
1 1 3.167 0.1
1 1 3.307 0.5
1 1 3.179 1.0
0 5 0.56 0
1 5 17.448 0.001
1 5 15.473 0.005
1 5 15.62 0.01
1 5 16.689 0.05
1 5 17.657 0.1
1 5 19.853 0.5
1 5 19.646 1.0
0 10 1.013 0
1 10 36.245 0.001
1 10 32.367 0.005
1 10 31.887 0.01
1 10 34.292 0.05
1 10 36.813 0.1
1 10 50.019 0.5
1 10 51.295 1.0
0 50 4.273 0
1 50 182.469 0.001
1 50 161.339 0.005
1 50 170.049 0.01
1 50 187.334 0.05
1 50 193.634 0.1
1 50 315.766 0.5
1 50 443.703 1.0
0 100 8.391 0
1 100 388.164 0.001
1 100 329.601 0.005
1 100 339.721 0.01
1 100 364.003 0.05
1 100 414.809 0.1
1 100 716.797 0.5
1 100 1049.77 1.0
0 500 43.426 0
1 500 1883.28 0.001
1 500 1637.86 0.005
1 500 1693.27 0.01
1 500 1826.02 0.05
1 500 2066.06 0.1
1 500 3591.17 0.5
1 500 5307.58 1.0
0 1000 78.268 0
1 1000 3444.88 0.001
1 1000 3012.46 0.005
1 1000 3047.89 0.01
1 1000 3292.1 0.05
1 1000 3651.14 0.1
1 1000 6307.4 0.5
1 1000 10140.9 1.0
0 5000 281.691 0
1 5000 11901.2 0.001
1 5000 11215.8 0.005
1 5000 11298.8 0.01
1 5000 12353.6 0.05
1 5000 14028.8 0.1
1 5000 24102.9 0.5
1 5000 37907.8 1.0
