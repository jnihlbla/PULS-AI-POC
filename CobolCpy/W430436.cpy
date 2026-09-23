000100 01  W430436.                                                             
000200*                                 POSTBESKRIVNING FÖR                     
000300*                                 PLOCKREGISTRET W43023                   
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 TIPLOCK              PIC S9(3)           COMP-3.                  
001200*                                 PLOCKNINGS DATUM                        
001300     03 FILLER               PIC X(10).                                   
001400     03 DEPFLT               PIC S9              COMP-3.                  
001500     03 PERDEL               OCCURS 8 TIMES.                              
001600*                                 PERIODDELAR                             
001700*                                                                         
001800        05 TIPER             PIC S9              COMP-3.                  
001900*                                 PERIODNUMMER                            
002000        05 KLASSDEL          OCCURS 4 TIMES.                              
002100*                                                                         
002200           07 KDORDKL        PIC S9              COMP-3.                  
002300*                                 ORDERKLASS                              
002400           07 SUPLOCK        PIC S9(5)           COMP-3.                  
002500*                                 SUMMA PLOCKADE RADER                    
002600           07 SUBEART-STYCK  PIC S9(7)           COMP-3.                  
002700*                                 SUMMA BESTÄLLT ANTAL                    
002800           07 SUBEART-Q1     PIC S9(7)           COMP-3.                  
002900*                                 SUMMA BESTÄLLT ANTAL                    
003000           07 SUBEART-Q2     PIC S9(7)           COMP-3.                  
003100*                                 SUMMA BESTÄLLT ANTAL                    
003200           07 SUBEART-Q3     PIC S9(7)           COMP-3.                  
003300*                                 SUMMA BESTÄLLT ANTAL                    
003400           07 SUBEART-Q4     PIC S9(7)           COMP-3.                  
003500*                                 SUMMA BESTÄLLT ANTAL                    
003600*** END COPY W430436     LENGTH=799                                       
