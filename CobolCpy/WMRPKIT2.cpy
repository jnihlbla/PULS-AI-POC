000100 01  MRP-KIT-WMRPKIT.                                                     
000200*                                 COPYTEXT FOR MRP INDIA LABEL            
000300     03 MRP-KIT-IDARTNR      PIC Z(7)9.                                   
000400*                                 ARTIKELNUMMER                           
000500     03 MRP-KIT-BEART-HEAD   PIC X(15).                                   
000600*                                 ARTIKELBENÄMNING      BEART-002         
000700     03 MRP-KIT-KVAVBART     PIC 9(7).                                    
000800*                                 AVBOKAT ANTAL ARTIKLAR                  
000900     03 MRP-KIT-KVRADER      PIC 9(5).                                    
001000*                                 ANTAL RADER                             
001100     03 MRP-KIT-MRPKIT       OCCURS 125 TIMES.                            
001200        05 MRP-KIT-BEART     PIC X(15).                                   
001300*                                 ARTIKELBENÄMNING      BEART-002         
001400        05 MRP-KIT-REANTPSA  PIC Z9.9(3).                                 
001500*                                 ANTAL PER SATS                          
001600        05 MRP-KIT-KDSORT    PIC X(2).                                    
001700*** END OF VILMAII-COPY LENGTH= 2910 BYTES                                
