000100 01  REQU-W60161I1.                                                       
000200*                                 UPPDATERING HANTERINGSKOD               
000300     03 REQU-KVRADER         PIC 9(5).                                    
000400*                                 ANTAL RADER                             
000500     03 REQU-IDDC            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 REQU-KDMFSFOR        PIC X.                                       
000800*                                 TYP AV MFS-FORMAT                       
000900*                                 1 = W-FORMAT  2 = N-FORMAT              
001000     03 REQU-INPUT-LINES     OCCURS 10 TIMES.                             
001100        05 REQU-IDARTNR      PIC 9(8).                                    
001200*                                 ARTIKELNUMMER                           
001300        05 REQU-KDARTHNT-V   PIC 9(3).                                    
001400*                                 HANTERINGSKOD VÄNSTRA DELEN             
001500        05 REQU-KDARTHNT-H   PIC 9(3).                                    
001600*                                 HANTERINGSKOD HÖGRA DELEN               
001700        05 REQU-KDARTURS     PIC X(2).                                    
001800*                                 ARTIKELURSPRUNGSKOD                     
001900*** END OF VILMAII-COPY LENGTH= 168 BYTES                                 
