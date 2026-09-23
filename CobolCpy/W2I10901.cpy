000100 01  MID-W2I10901.                                                        
000200*                                                                         
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-TIAAVV-IN        PIC X(4).                                    
000800*                                 ≈R - VECKA  (≈≈VV)                      
000900     03 MID-TIAAVV-UT        PIC X(4).                                    
001000*                                 ≈R - VECKA  (≈≈VV)                      
001100     03 MID-TID-IN           PIC X.                                       
001200*                                 DAGNUMMER I VECKA (M≈NDAG = 1)          
001300     03 MID-TID-UT           PIC X.                                       
001400*                                 DAGNUMMER I VECKA (M≈NDAG = 1)          
001500     03 MID-INAREA.                                                       
001600*                                 ORDERING≈NG  INPUT                      
001700        05 MID-KVOI-PROG-VV-IN                                            
001800                             PIC X(7).                                    
001900*                                 ORDERING≈NG PROGNOSP≈VERKANDE           
002000        05 MID-KVOI-PROG-DD-IN                                            
002100                             PIC X(7).                                    
002200*                                 ORDERING≈NG PROGNOSP≈VERKANDE           
002300        05 MID-KVOI-DIV-VV-IN                                             
002400                             PIC X(7).                                    
002500*                                 ORDERING≈NG DIVERSE OCH TPO             
002600        05 MID-KVOI-DIV-DD-IN                                             
002700                             PIC X(7).                                    
002800*                                 ORDERING≈NG DIVERSE OCH TPO             
002900        05 MID-KVOI-SATS-VV-IN                                            
003000                             PIC X(7).                                    
003100*                                 ORDERING≈NG SATSF÷RBRUKNING             
003200        05 MID-KVOI-SATS-DD-IN                                            
003300                             PIC X(7).                                    
003400*                                 ORDERING≈NG SATSF÷RBRUKNING             
003500        05 MID-KVOI-SDC-VV-IN                                             
003600                             PIC X(7).                                    
003700*                                 ORDERING≈NG LEV FR≈N SDC                
003800        05 MID-KVOI-SDC-DD-IN                                             
003900                             PIC X(7).                                    
004000*                                 ORDERING≈NG LEV FR≈N SDC                
004100        05 MID-KVOI-NDC-VV-IN                                             
004200                             PIC X(7).                                    
004300*                                 ORDERING≈NG LEV FR≈N NDC                
004400        05 MID-KVOI-NDC-DD-IN                                             
004500                             PIC X(7).                                    
004600*                                 ORDERING≈NG LEV FR≈N NDC                
004700        05 MID-KVOI-LED-VV-IN                                             
004800                             PIC X(7).                                    
004900*                                 ORDERING≈NG F÷RSKUTEN                   
005000        05 MID-KVOI-LED-DD-IN                                             
005100                             PIC X(7).                                    
005200*                                 ORDERING≈NG F÷RSKUTEN                   
005300        05 MID-KVOI-REF-VV-IN                                             
005400                             PIC X(7).                                    
005500*                                 ORDERING≈NG LEV FR≈N REFILL             
005600        05 MID-KVOI-REF-DD-IN                                             
005700                             PIC X(7).                                    
005800*                                 ORDERING≈NG LEV FR≈N REFILL             
005900     03 MID-KDBEHX           PIC X.                                       
006000*** END OF VILMAII-COPY LENGTH= 127 BYTES                                 
