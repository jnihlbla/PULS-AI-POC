000100 01  9310-WDGX9310.                                                       
000200*                                 STYRANDE VAT KOD                        
000300*                                 FYSISK NYCKEL: KY9310                   
000400*                                 (IDLANDX2, KDVAT)                       
000500*                                                                         
000600     03 9310-IDLANDX2        PIC X(2).                                    
000700*                                 2-STƒLLIG LANDSBETECKNINGSKOD           
000800*                                 2-LETTER CODE FOR COUNTRY               
000900     03 9310-KDVAT           PIC X(2).                                    
001000*                                 MOMSKOD                                 
001100*                                 VAT CODE                                
001200     03 9310-REVAT           PIC S9(3)V9(2)      COMP-3.                  
001300*                                 MULTIPLIKATIONSFAKTOR F÷R MOMS          
001400*                                 VAT FACTOR                              
001500     03 9310-BEVAT           PIC X(50).                                   
001600*                                 MOMSKODSBENƒMNING R3                    
001700*                                 VAT CODE DESCRIPTION R3                 
001800     03 9310-DAREGDAT        PIC 9(8).                                    
001900*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
002000*                                 REGISTRATION DATE (YYYYMMDD)            
002100     03 9310-DAUPPDAT        PIC 9(8).                                    
002200*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
002300*                                 UPDATING DATE     (YYYYMMDD)            
002400     03 9310-DADELDAT        PIC 9(8).                                    
002500*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
002600*                                 DELETION DATE     (YYYYMMDD)            
002700     03 9310-IDUSER          PIC X(8).                                    
002800*                                 ANVƒNDARENS SƒKERHETS ID                
002900*                                 USER SECURITY-IDENTITY                  
003000*** END OF VILMAII-COPY LENGTH= 89 BYTES                                  
