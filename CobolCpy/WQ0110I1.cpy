000100 01  REQU-WQ0110I1.                                                       
000200*                                 MID-COPYTEXT FOR WQ011000               
000300     03 REQU-IDELMT          OCCURS 15 TIMES                              
000400                             PIC X(16).                                   
000500*                                 DATAELEMENTIDENTITET                    
000600*                                 DATA ITEM NAME                          
000700     03 REQU-SEARCH-TABLE    OCCURS 15 TIMES.                             
000800*                                                                         
000900        05 REQU-IDELMT-SEARCH                                             
001000                             PIC X(16).                                   
001100*                                 DATAELEMENTIDENTITET                    
001200*                                 DATA ITEM NAME                          
001300        05 REQU-TEELMTVAL-SEARCH                                          
001400                             PIC X(1000).                                 
001500*                                 LISTA AV VÄRDEN                         
001600*                                 LIST OF VALUES                          
001700*** END OF VILMAII-COPY LENGTH= 15480 BYTES                               
