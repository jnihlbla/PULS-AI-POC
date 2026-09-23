000010*** EDIT ALLOWED                                                          
000100 01  W271LARM.                                                            
000110                                                                          
000120*    COPYTEXTEN ANVÄNDS INTE.                                             
000130*                                                                         
000140*    HIGH-SALES BERÄKNING SKER EFTER REGLE I PGM                          
000141*    W23272, W27256 OCH W27158, SK                                        
000142*                                                                         
000150*                                                                         
000200*                                 TABELL FAKTORER ONORMAL FÖR-            
000300*                                 SÄLJNING. ANVÄNDS FÖR ATT               
000400*                                 LARMA OM ONOMALT HÖG FSG I              
000500*                                 S-LAGER - DAGLIGEN.                     
000600*                                 (A * PROGNOS) + B                       
000700*                                                                         
000800     03 S1-PRISKLASSER.                                                   
000900        05 FILLER PIC X(15) VALUE '000000050 02525'.                      
001000        05 FILLER PIC X(15) VALUE '000000250 02115'.                      
001100        05 FILLER PIC X(15) VALUE '000001000 01808'.                      
001200        05 FILLER PIC X(15) VALUE '000005000 01605'.                      
001300        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
001400        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
001500        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
001600                                                                          
001700     03 S2-PRISKLASSER.                                                   
001800        05 FILLER PIC X(15) VALUE '000000050 02525'.                      
001900        05 FILLER PIC X(15) VALUE '000000250 02115'.                      
002000        05 FILLER PIC X(15) VALUE '000001000 01808'.                      
002100        05 FILLER PIC X(15) VALUE '000005000 01605'.                      
002200        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002300        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002400        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002500                                                                          
002510     03 S3-PRISKLASSER.                                                   
002520        05 FILLER PIC X(15) VALUE '000000050 02525'.                      
002530        05 FILLER PIC X(15) VALUE '000000250 02115'.                      
002540        05 FILLER PIC X(15) VALUE '000001000 01808'.                      
002550        05 FILLER PIC X(15) VALUE '000005000 01605'.                      
002560        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002570        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002580        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002590                                                                          
002591     03 S4-PRISKLASSER.                                                   
002592        05 FILLER PIC X(15) VALUE '000000050 02525'.                      
002593        05 FILLER PIC X(15) VALUE '000000250 02115'.                      
002594        05 FILLER PIC X(15) VALUE '000001000 01808'.                      
002595        05 FILLER PIC X(15) VALUE '000005000 01605'.                      
002596        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002597        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002598        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002599                                                                          
002600     03 S5-PRISKLASSER.                                                   
002601        05 FILLER PIC X(15) VALUE '000000050 02525'.                      
002602        05 FILLER PIC X(15) VALUE '000000250 02115'.                      
002603        05 FILLER PIC X(15) VALUE '000001000 01808'.                      
002604        05 FILLER PIC X(15) VALUE '000005000 01605'.                      
002605        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
002606        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
002607        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002608                                                                          
002610                                                                          
002700 01  FILLER REDEFINES W271LARM.                                           
002800     03 S-LARMFAKT         OCCURS 5.                                      
002900        05 S-LARM             OCCURS 7.                                   
003000           07 S-PRARTSTD-MAX  PIC 9(7)V9(2).                              
003100           07 FILLER          PIC X.                                      
003200           07 A               PIC 9(2)V9.                                 
003300           07 B               PIC 9(2).                                   
003400                                                                          
003500*** END COPY W271LARM    LENGTH=315                                       
