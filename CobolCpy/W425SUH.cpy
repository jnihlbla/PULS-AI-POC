000100 01  W425SUH.                                                             
000200*                                 INFORMATION OM SOFTWARE                 
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 IDSUPPL              PIC S9(5)           COMP-3.                  
001100*                                 LEVERANSNR TILL ≈TERF÷RSƒLJARE          
001200     03 IDFAKT               PIC S9(7)           COMP-3.                  
001300*                                 FAKTURANUMMER                           
001400     03 IDORDNR              PIC S9(7)           COMP-3.                  
001500*                                 ORDERNR             IDORDNR-002         
001600     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001700*                                 KOLLINUMMER                             
001800     03 IDARTNR              PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 IDRONR               PIC S9(7)           COMP-3.                  
002100*                                 RESTORDERNUMMER      IDRONR-002         
002200     03 KDRO                 PIC S9              COMP-3.                  
002300*                                 RESTORDERKOD P≈ INFORMATION             
002400*                                 TILL VR                                 
002500     03 KDORDER              PIC S9              COMP-3.                  
002600*                                 ORDERKOD                                
002700     03 BERADREF             PIC X(10).                                   
002800*                                 KUNDENS RADREFERENS                     
002900     03 KDFAKTYP             PIC X.                                       
003000*                                 FAKTURATYP                              
003100     03 KDVRINFO             PIC S9              COMP-3.                  
003200*                                 P≈VERKAN I VR/DSP SYSTEM                
003300     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
003400*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
003500     03 TIKLOCK              PIC S9(9)           COMP-3.                  
003600*                                 KLOCKSLAG (TTMMSSTH)                    
003700     03 IDKLIENT             PIC X(10).                                   
003800*                                 VADIS KLIENT                            
003900     03 IDARBREF             PIC X(10).                                   
004000*                                 ARBETSORDER VADIS                       
004100     03 IDBIL.                                                            
004200*                                 BILIDENTITET                            
004300        05 IDBILTYP          PIC X(3).                                    
004400*                                 BILTYP                                  
004500        05 TIAAAA            PIC X(4).                                    
004600*                                 ≈RTAL (≈≈≈≈)                            
004700        05 IDCHASSI-PIE      PIC X(6).                                    
004800*                                 CHASSINUMMER PIE                        
004900     03 IDVIN                PIC X(17).                                   
005000*                                 VIN ID FORDON                           
005100     03 FILLER               PIC X(5).                                    
005200*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
