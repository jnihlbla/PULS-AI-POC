000100 01  REQU-WL0602I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WL0601             
000300*                                 LDC INBOUND LOCATION QUERY              
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-ADLAGOMR-KEY    PIC 9(2).                                    
000800*                                 LAGEROMRÅDE                             
000900*                                 AREA                                    
001000     03 REQU-ADGANG-KEY      PIC 9(2).                                    
001100*                                 GÅNG                                    
001200*                                 AISLE                                   
001300     03 REQU-BAY-KEY         PIC 9(2).                                    
001400     03 REQU-FL-NOEXIST      PIC X.                                       
001500*                                 JA/NEJ-FLAGGA                           
001600     03 REQU-FL-DEVIATION    PIC X.                                       
001700*                                 JA/NEJ-FLAGGA                           
001800     03 REQU-FL-COVER        PIC X.                                       
001900*                                 JA/NEJ-FLAGGA                           
002000     03 REQU-KVRADER         PIC 9(5).                                    
002100*                                 ANTAL RADER                             
002200*                                 NUMBER OF LINES                         
002300*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
