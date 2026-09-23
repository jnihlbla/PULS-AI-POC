000100 01  RESP-WL0169O1.                                                       
000200*                                 RESPONS FROM PGM WL0169                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-KVANTAL-UTSKR   PIC Z(5)9.                                   
000600*                                 ANTAL                                   
000700     03 RESP-KVANTAL-EJUTSKR PIC Z(5)9.                                   
000800*                                 ANTAL                                   
000900     03 RESP-KVRADER         PIC Z(4)9.                                   
001000*                                 ANTAL RADER                             
001100     03 RESP-OMR-VVKL-GRP    OCCURS 500 TIMES.                            
001200*                                 VOLYMVÄRDESKLASS PER OMRÅDE             
001300        05 RESP-KDVVKL       PIC 9.                                       
001400*                                 VOLYMVÄRDESKLASS                        
001500        05 RESP-ADLAGOMR     PIC 9(2).                                    
001600*                                 LAGEROMRÅDE                             
001700        05 RESP-KVANTAL      PIC Z(5)9.                                   
001800*                                 ANTAL                                   
001900*** END OF VILMAII-COPY LENGTH= 4519 BYTES                                
