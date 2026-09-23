000100 01  REQU-WL0176I1.                                                       
000200*                                 REQUEST TO PGM WL0176                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-INV-RE1-GRP     OCCURS 24 TIMES.                             
000600*                                 INVENTERINGSARTIKELGRUPP                
000700        05 REQU-IDARTNR      PIC 9(8).                                    
000800*                                 ARTIKELNUMMER                           
000900        05 REQU-KDINVPRIO    PIC 9.                                       
001000*                                 INVENTERING PRIORITET                   
001100        05 REQU-TEINVANM     PIC X(25).                                   
001200*                                 INVENTERINGSANMÄRKNING                  
001300*** END OF VILMAII-COPY LENGTH= 818 BYTES                                 
