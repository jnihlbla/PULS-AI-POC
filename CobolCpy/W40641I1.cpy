000100 01  REQU-W40641I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM W4W641             
000300*                                 OUTBOUND CHANGE IDLBBET                 
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-TIFAKT-KEY      PIC X(6).                                    
000800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
000900*                                 INVOICING DATE   (YYMMDD)               
001000     03 REQU-IDLBBET-KEY     PIC X(12).                                   
001100*                                 LASTBÄRARBETECKNING                     
001200*                                 TRAILER NUMBER                          
001300     03 REQU-IDDC-REC-KEY    PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 REQU-KVRADER         PIC 9(5).                                    
001700*                                 ANTAL RADER                             
001800*                                 NUMBER OF LINES                         
001900     03 REQU-TABELLRAD       OCCURS 500 TIMES.                            
002000*                                 GRUPP MED TABELLRADER                   
002100        05 REQU-IDFAKT       PIC 9(7).                                    
002200*                                 FAKTURANUMMER                           
002300*                                 INVOICE NO.                             
002400        05 REQU-TIBERANK     PIC 9(6).                                    
002500*                                 BERÄKNAD ANKOMSTDATUM                   
002600*                                 ESTIMATED RECEIVING DATE                
002700        05 REQU-IDLBBET      PIC X(12).                                   
002800*                                 LASTBÄRARBETECKNING                     
002900*                                 TRAILER NUMBER                          
003000*** END OF VILMAII-COPY LENGTH= 12527 BYTES                               
