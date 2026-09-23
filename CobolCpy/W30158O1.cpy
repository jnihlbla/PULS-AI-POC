000100 01  RESP-W30158O1.                                                       
000200*                                 RESPONS FROM PGM W30158                 
000300     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
000400*                                 ARTIKELNUMMER                           
000500*                                 PART NUMBER                             
000600     03 RESP-IDARTNR-OBJ-KEY PIC Z(7)9.                                   
000700*                                 OBJEKTNUMMER                            
000800     03 RESP-KVRADER         PIC Z(4)9.                                   
000900*                                 ANTAL RADER                             
001000*                                 NUMBER OF LINES                         
001100     03 RESP-RADER           OCCURS 500 TIMES.                            
001200*                                                                         
001300        05 RESP-IDARTNR      PIC Z(7)9.                                   
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600        05 RESP-BEART        PIC X(25).                                   
001700*                                 ARTIKELBENÄMNING                        
001800*                                 PART DESCRIPTION                        
001900*** END OF VILMAII-COPY LENGTH= 16521 BYTES                               
