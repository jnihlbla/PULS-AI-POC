000100 01  W221W001.                                                            
000200*                                 KALENDER F÷R ANSKAFFNINGEN              
000300*                                 DATASET INNEH≈LLER MINST 105            
000400*                                 VECKOR FRAM≈T (MAX 255) (F÷RSTA         
000500*                                 VECKAN SKALL VARA PERIODVECKA           
000600*                                 1)                                      
000700*                                                                         
000800     03 IDPTYP               PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 FILLER               PIC X(2).                                    
001100     03 TIAAR                PIC 9(2).                                    
001200*                                 ≈R    (≈≈)                              
001300     03 FILLER               PIC X(3).                                    
001400     03 TIVECKNR             PIC 9(2).                                    
001500*                                 VECKONUMMER (VV)                        
001600     03 FILLER               PIC X(2).                                    
001700     03 TIPER                PIC 9.                                       
001800*                                 PERIODNUMMER                            
001900     03 FILLER               PIC X(3).                                    
002000     03 TIVECKNR-PERIOD      PIC 9(2).                                    
002100*                                 VECKONUMMER (VV)                        
002200     03 FILLER               PIC X(3).                                    
002300     03 TIMAANAD             PIC 9(2).                                    
002400*                                 M≈NAD (MM)                              
002500     03 FILLER               PIC X(3).                                    
002600     03 TIVECKNR-MAANAD      PIC 9.                                       
002700*                                 VECKA (V)          TIVECKNR-002         
002800     03 FILLER               PIC X.                                       
002900     03 FLAGGA-SEMESTER      PIC X.                                       
003000*                                 SEMESTERVECKA                           
003100     03 FLAGGA-QF            PIC X.                                       
003200*                                 QF-VECKA                                
003300*** END COPY W221W001C0  LENGTH=32                                        
