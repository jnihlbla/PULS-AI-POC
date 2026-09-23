000100 01  REQU-WB0101I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WB0101             
000300*                                 ACCESSORIES SHOW LIST                   
000400     03 REQU-TIAOINF-AAVV-FOM-KEY                                         
000500                             PIC X(4).                                    
000600*                                 DATUM ÄO-INFÖRANDE                      
000700*                                 CO INTRODUCTION WEEK                    
000800     03 REQU-TIAOINF-AAVV-TOM-KEY                                         
000900                             PIC X(4).                                    
001000*                                 DATUM ÄO-INFÖRANDE                      
001100*                                 CO INTRODUCTION WEEK                    
001200     03 REQU-IDUPPDKU-KEY    PIC 9(8).                                    
001300*                                 KU-UPPDRAGSNUMMER TIKO                  
001400*                                 KU COMMISSION IDENTITY NO               
001500     03 REQU-IDUPPDSU-KEY    PIC 9(8).                                    
001600*                                 SU-UPPDRAGSNUMMER TIKO                  
001700*                                 SU COMMISSION IDENTITY NO               
001800     03 REQU-IDARTNR-KEY     PIC X(8).                                    
001900*                                 ARTIKELNUMMER                           
002000*                                 PART NUMBER                             
002100     03 REQU-BEUPPDSU-KEY    PIC X(35).                                   
002200*                                 SU-UPPDRAG BENÄMNING                    
002300*                                 SU COMMISSION DESCRIPTION               
002400     03 REQU-KVRADER-W-START PIC 9(5).                                    
002500*                                 ANTAL RADER                             
002600*                                 NUMBER OF LINES                         
002700     03 REQU-KVRADER-W-VISAS PIC 9(5).                                    
002800*                                 ANTAL RADER                             
002900*                                 NUMBER OF LINES                         
003000     03 REQU-KVRADER-W-TOTAL PIC 9(5).                                    
003100*                                 ANTAL RADER                             
003200*                                 NUMBER OF LINES                         
003300     03 REQU-KVRADER-D-TOTAL PIC 9(5).                                    
003400*                                 ANTAL RADER                             
003500*                                 NUMBER OF LINES                         
003600     03 REQU-KVRADER-D-SIST  PIC 9(5).                                    
003700*                                 ANTAL RADER                             
003800*                                 NUMBER OF LINES                         
003900*** END OF VILMAII-COPY LENGTH= 92 BYTES                                  
