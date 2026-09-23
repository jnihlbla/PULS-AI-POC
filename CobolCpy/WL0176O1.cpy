000100 01  RESP-WL0176O1.                                                       
000200*                                 RESPONS TO PGM WL0176                   
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-RAD             OCCURS 24 TIMES.                             
000600        05 RESP-IDARTNR      PIC Z(7)9.                                   
000700*                                 ARTIKELNUMMER                           
000800        05 RESP-KDINVPRIO    PIC 9.                                       
000900*                                 INVENTERING PRIORITET                   
001000        05 RESP-TEINVANM     PIC X(25).                                   
001100*                                 INVENTERINGSANMÄRKNING                  
001200        05 RESP-IDMSG-ERROR-LINE                                          
001300                             PIC X(3).                                    
001400*                                 FELMEDDELANDE ID                        
001500*** END OF VILMAII-COPY LENGTH= 890 BYTES                                 
