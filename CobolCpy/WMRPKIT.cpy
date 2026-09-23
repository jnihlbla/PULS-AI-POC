000100 01  MRP-KIT-WMRPKIT.                                                     
000200*                                 COPYTEXT FOR MRP INDIA LABEL            
000300     03 MRP-KIT-IDAFPRCD     PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500     03 MRP-KIT-IDARTNR      PIC Z(7)9.                                   
000600*                                 ARTIKELNUMMER                           
000700     03 MRP-KIT-BEART-HEAD   PIC X(15).                                   
000800*                                 ARTIKELBENÄMNING      BEART-002         
000900     03 MRP-KIT-MRPKIT       OCCURS 10 TIMES.                             
001000        05 MRP-KIT-BEART     PIC X(15).                                   
001100*                                 ARTIKELBENÄMNING      BEART-002         
001200        05 MRP-KIT-REANTPSA  PIC X(6).                                    
001300*                                 ANTAL PER SATS                          
001400        05 MRP-KIT-KDSORT    PIC X(2).                                    
001500     03 MRP-KIT-LABEL-COUNT  PIC X(10).                                   
001600*** END OF VILMAII-COPY LENGTH= 273 BYTES                                 
