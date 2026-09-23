000100 01  RESP-WL0170O1.                                                       
000200*                                 RESPONS FROM PGM WL0170                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
000600*                                 ARTIKELNUMMER                           
000700     03 RESP-SUINVJUST       PIC -(9)9.                                   
000800*                                 JUSTERAD SUMMERAD KVANTITET             
000900     03 RESP-COL             OCCURS 48 TIMES.                             
001000        05 RESP-TIJUSTDA     PIC 9(5).                                    
001100*                                 JUSTERINGSDATUM                         
001200        05 RESP-KDJUSTYP     PIC X(2).                                    
001300        05 RESP-KVJUSTKV     PIC -(6)9.                                   
001400*                                 JUSTERAD KVANTITET                      
001500        05 RESP-IDUSER-CLO   PIC X(8).                                    
001600*                                 ANVÄNDAR-ID VID AVSLUT                  
001700*** END OF VILMAII-COPY LENGTH= 1076 BYTES                                
