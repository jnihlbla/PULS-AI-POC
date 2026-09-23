000100 01  W3300004.                                                            
000200*                                 ARTIKELSTATISTIK                        
000300*                                 DISTRIKTSRECORD                         
000400*                                 RESTER FRÅN DEN GAMLA                   
000500*                                 FÖRSÄLJNINGSSTATISTIKEN                 
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 IDDISTR              PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 FLSPECPR             PIC X.                                       
001100*                                 SPECIALPRISFLAGGA                       
001200     03 KVPERIOD             PIC S9(3)           COMP-3.                  
001300*                                 ANTAL PERIODER                          
001400     03 SUM-GRP              OCCURS 16 TIMES.                             
001500*                                 ARTIKELRECORD                           
001600        05 TIANTPER          PIC S9(3)           COMP-3.                  
001700*                                 FÖRS.PERIODER                           
001800        05 SULEVANT          PIC S9(9)           COMP-3.                  
001900*                                 SUMMA LEVERERAT ANTAL                   
002000*                                 AV 1 ARTIKEL                            
002100        05 SUARTFSG          PIC S9(9)V9(2)      COMP-3.                  
002200*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
002300*                                                                         
002400        05 SUARTSJK          PIC S9(9)V9(2)      COMP-3.                  
002500*                                 SUMMA SJÄLVKOSTNAD ARTIKELNR            
002600*** END COPY W3300004C0  LENGTH=313                                       
