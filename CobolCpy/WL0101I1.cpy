000100 01  REQU-WL0101I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WL0101             
000300*                                 LDC GOODS RECEIVING                     
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-TIFAKT-KEY      PIC X(6).                                    
000800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
000900*                                 INVOICING DATE   (YYMMDD)               
001000     03 REQU-IDLBBET-KEY     PIC X(12).                                   
001100*                                 LASTBÄRARBETECKNING                     
001200*                                 TRAILER NUMBER                          
001300     03 REQU-IDDC-SEND-KEY   PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 REQU-KVRADER         PIC 9(5).                                    
001700*                                 ANTAL RADER                             
001800*                                 NUMBER OF LINES                         
001900     03 REQU-TABELLRAD       OCCURS 500 TIMES.                            
002000*                                 GRUPP MED TABELLRADER                   
002100        05 REQU-CMD-IN       PIC X(3).                                    
002200        05 REQU-IDFAKT       PIC 9(7).                                    
002300*                                 FAKTURANUMMER                           
002400*                                 INVOICE NO.                             
002500        05 REQU-TIBERANK     PIC 9(6).                                    
002600*                                 BERÄKNAD ANKOMSTDATUM                   
002700*                                 ESTIMATED RECEIVING DATE                
002800        05 REQU-ADINLOMR     PIC X(4).                                    
002900*                                 INLEVERANSOMRÅDE                        
003000*                                 RECEIVING AREA                          
003100        05 REQU-IDTRACK      PIC X(25).                                   
003200*                                 TRACKING ID FROM CUSTOMS                
003300*                                 CUSTOMS TRACKING ID                     
003400*** END OF VILMAII-COPY LENGTH= 22527 BYTES                               
