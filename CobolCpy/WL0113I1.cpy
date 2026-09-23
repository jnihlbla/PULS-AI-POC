000100 01  REQU-WL0113I1.                                                       
000200*                                 REQUEST TO PGM WL0113                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDARTNR-KEY     PIC 9(8).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 REQU-IDDC2-KEY       PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 REQU-SHOW-KEY        PIC X.                                       
001000*                                 ALLMÄN FLAGGA                           
001100     03 REQU-BELANG-KEY      PIC X(20).                                   
001200*                                 SPRÅK I KLARTEXT                        
001300     03 REQU-KVRADER         PIC 9(5).                                    
001400*                                 ANTAL RADER                             
001500     03 REQU-INPUT.                                                       
001600*                                                                         
001700        05 REQU-RAD          OCCURS 500 TIMES.                            
001800*                                                                         
001900           07 REQU-IDDC      PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100           07 REQU-KDLEVSP-UPD                                            
002200                             PIC 9(2).                                    
002300*                                 SPÄRRKOD LEVERANS                       
002400           07 REQU-KVSPARR-KVAL-UPD                                       
002500                             PIC 9(7).                                    
002600*                                 SPÄRRAT ANTAL KVALITETSFEL              
002700           07 REQU-TEKVAL    PIC X(10).                                   
002800*                                 KVALITETSNOTERING SPÄRR                 
002900*** END OF VILMAII-COPY LENGTH= 10538 BYTES                               
