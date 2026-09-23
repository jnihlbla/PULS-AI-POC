000100 01  REQU-WL0166I1-CTX.                                                   
000200*                                 REQUEST TO PGM WL0166                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 REQU-IDORDNR-KEY     PIC 9(5).                                    
001000*                                 ORDERNUMMER                             
001100     03 REQU-FLANNULL        PIC X.                                       
001200*                                 ANNULLATION                             
001300     03 REQU-KVRADER         PIC 9(5).                                    
001400*                                 ANTAL RADER                             
001500     03 REQU-WL0166I1-001-GRP                                             
001600                             OCCURS 100 TIMES.                            
001700        05 REQU-IDARTNR-006  PIC 9(8).                                    
001800*                                 ARTIKELNUMMER                           
001900        05 REQU-KVBEART      PIC 9(6).                                    
002000*                                 BESTÄLLT ANTAL STYCKEN                  
002100        05 REQU-BERADREF     PIC X(10).                                   
002200*                                 KUNDENS RADREFERENS                     
002300        05 REQU-FLINVEST     PIC X.                                       
002400*                                 BYTES INVENTERINGSFLAGGA                
002500*** END OF VILMAII-COPY LENGTH= 2523 BYTES                                
