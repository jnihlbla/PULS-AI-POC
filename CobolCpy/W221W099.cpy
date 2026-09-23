000010*** EDIT ALLOWED                                                          
000100************************************************************              
000200*                                                                         
000300*    TABELL FÖR UPPDELNING AV ARTIKLAR I                                  
000400*           PRISKLASS  OCH                                                
000500*           FREKVENSKLASS                                                 
000600*    (INGÅR I PGM W22199)                                                 
000700*                                                                         
000800************************************************************              
000900                                                                          
000901*               STANDARDPRIS (PRARTSTD)   MINDRE ELLER LIKA MED           
000920                                                                          
001000 01  TABELL-PRISKLASS-PRIS.                                               
001100     03  FILLER             PIC S9(7)V99 COMP-3 VALUE      1.00.          
001200     03  FILLER             PIC S9(7)V99 COMP-3 VALUE      3.00.          
001300     03  FILLER             PIC S9(7)V99 COMP-3 VALUE     10.00.          
001400     03  FILLER             PIC S9(7)V99 COMP-3 VALUE     30.00.          
001500     03  FILLER             PIC S9(7)V99 COMP-3 VALUE    100.00.          
001600     03  FILLER             PIC S9(7)V99 COMP-3 VALUE    300.00.          
001700     03  FILLER             PIC S9(7)V99 COMP-3 VALUE   1000.00.          
001800     03  FILLER             PIC S9(7)V99 COMP-3 VALUE   3000.00.          
001900                                                                          
002000 01  FILLER  REDEFINES TABELL-PRISKLASS-PRIS.                             
002100     03  TAB-PRIS OCCURS  8 PIC S9(7)V99 COMP-3.                          
002200                                                                          
002300 01  TABELL-PRISKLASS-KLASS.                                              
002400     03  FILLER             PIC X VALUE '1'.                              
002500     03  FILLER             PIC X VALUE '2'.                              
002600     03  FILLER             PIC X VALUE '3'.                              
002700     03  FILLER             PIC X VALUE '4'.                              
002800     03  FILLER             PIC X VALUE '5'.                              
002900     03  FILLER             PIC X VALUE '6'.                              
003000     03  FILLER             PIC X VALUE '7'.                              
003100     03  FILLER             PIC X VALUE '8'.                              
003200                                                                          
003300*    ARTIKLAR MED STDPRIS > 3000 KR HAR PRISKLASS = 9                     
003400                                                                          
003500 01  FILLER  REDEFINES TABELL-PRISKLASS-KLASS.                            
003600     03  TAB-KDPRISKL OCCURS 8  PIC X.                                    
003700                                                                          
003800 01  PKL-INDEX.                                                           
003900     03  IX-PKL             PIC S9(3) COMP-3  VALUE +1.                   
004000     03  MAX-IX-PKL         PIC S9(3) COMP-3  VALUE +8.                   
004100                                                                          
004101************************************************************              
004110                                                                          
004200*                                  STYCKFÖRSÄLJNING / ÅR ( =< )           
004300*                           ( PB-SEP + PB-SATS + PB-TPO ) * 12            
004400                                                                          
004500 01  TABELL-FREKVENSKLASS-ANTAL.                                          
004600     03  FILLER             PIC S9(7)    COMP-3 VALUE      9.             
004700     03  FILLER             PIC S9(7)    COMP-3 VALUE     26.             
004800     03  FILLER             PIC S9(7)    COMP-3 VALUE     87.             
004900     03  FILLER             PIC S9(7)    COMP-3 VALUE    260.             
005000     03  FILLER             PIC S9(7)    COMP-3 VALUE    867.             
005100     03  FILLER             PIC S9(7)    COMP-3 VALUE   2600.             
005200     03  FILLER             PIC S9(7)    COMP-3 VALUE   8700.             
005300     03  FILLER             PIC S9(7)    COMP-3 VALUE  27000.             
005310     03  FILLER             PIC S9(7)    COMP-3 VALUE  87000.             
005400                                                                          
005500 01  FILLER  REDEFINES TABELL-FREKVENSKLASS-ANTAL.                        
005600     03  TAB-ANT   OCCURS  9 PIC S9(7) COMP-3.                            
005700                                                                          
005800 01  TABELL-FREKVENSKLASS-KLASS.                                          
005900     03  FILLER             PIC X VALUE 'A'.                              
005910     03  FILLER             PIC X VALUE 'B'.                              
005920     03  FILLER             PIC X VALUE 'C'.                              
005930     03  FILLER             PIC X VALUE 'D'.                              
005940     03  FILLER             PIC X VALUE 'E'.                              
005950     03  FILLER             PIC X VALUE 'F'.                              
005960     03  FILLER             PIC X VALUE 'G'.                              
005970     03  FILLER             PIC X VALUE 'H'.                              
005980     03  FILLER             PIC X VALUE 'I'.                              
006700                                                                          
006800*    ARTIKLAR MED STYCKFÖRSÄLJNING / ÅR > 87000                           
006810*                 HAR FREKVENSKLASS = 'J'                                 
006900                                                                          
007000 01  FILLER  REDEFINES TABELL-FREKVENSKLASS-KLASS.                        
007100     03  TAB-KDFREKKL OCCURS 9  PIC X.                                    
007200                                                                          
007300 01  FKL-INDEX.                                                           
007400     03  IX-FKL             PIC S9(3) COMP-3  VALUE +1.                   
007500     03  MAX-IX-FKL         PIC S9(3) COMP-3  VALUE +9.                   
