000100 01  MID-W4I65401.                                                        
000200*                                 MID-COPYTEXT F÷R BILD  4654             
000300*                                 ORDER PLANNING PER PRC                  
000400     03 MID-IDDC-IN          PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 MID-IDDC-UT          PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 MID-TIRFSDAT-IN      PIC 9(6).                                    
000900*                                 KLART F÷R TRANSPORT ≈≈MMDD              
001000     03 MID-TIRFSDAT-UT      PIC 9(6).                                    
001100*                                 KLART F÷R TRANSPORT ≈≈MMDD              
001200     03 MID-IDPRC-FR-IN.                                                  
001300*                                 PRODUKTIONSKANAL                        
001400        05 MID-IDPRCBAS      PIC X(3).                                    
001500*                                 PRC-BAS                                 
001600        05 MID-IDPRCVAR      PIC X.                                       
001700*                                 PRC-VARIANT                             
001800     03 MID-IDPRC-FR-UT.                                                  
001900*                                 PRODUKTIONSKANAL                        
002000        05 MID-IDPRCBAS      PIC X(3).                                    
002100*                                 PRC-BAS                                 
002200        05 MID-IDPRCVAR      PIC X.                                       
002300*                                 PRC-VARIANT                             
002400     03 MID-IDPRC-TO-IN.                                                  
002500*                                 PRODUKTIONSKANAL                        
002600        05 MID-IDPRCBAS      PIC X(3).                                    
002700*                                 PRC-BAS                                 
002800        05 MID-IDPRCVAR      PIC X.                                       
002900*                                 PRC-VARIANT                             
003000     03 MID-IDPRC-TO-UT.                                                  
003100*                                 PRODUKTIONSKANAL                        
003200        05 MID-IDPRCBAS      PIC X(3).                                    
003300*                                 PRC-BAS                                 
003400        05 MID-IDPRCVAR      PIC X.                                       
003500*                                 PRC-VARIANT                             
003600*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
