000100 01  WF10M17.                                                             
000200*                                 MOMSKODER FR≈N SAP R/3                  
000300     03 IDLEGSEL             PIC X(4).                                    
000400*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000500*                                 LEGAL SELLER IDENTITY                   
000600     03 IDLAND               PIC X(2).                                    
000700*                                 2-STƒLLIG LANDSBETECKNINGSKOD           
000800*                                 2-LETTER CODE FOR COUNTRY               
000900     03 KDVAT                PIC X(2).                                    
001000*                                 MOMSKOD                                 
001100*                                 VAT CODE                                
001200     03 REVAT                PIC 9(3)V9(2).                               
001300*                                 MULTIPLIKATIONSFAKTOR F÷R MOMS          
001400*                                 VAT FACTOR                              
001500     03 BEVAT                PIC X(50).                                   
001600*                                 MOMSKODSBENƒMNING R3                    
001700*                                 VAT CODE DESCRIPTION R3                 
001800     03 DAREGDAT             PIC 9(8).                                    
001900*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
002000*                                 REGISTRATION DATE (YYYYMMDD)            
002100     03 DAUPPDAT             PIC 9(8).                                    
002200*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
002300*                                                                         
002400*                                 UPDATING DATE     (YYYYMMDD)            
002500*                                                                         
002600     03 DADELDAT             PIC 9(8).                                    
002700*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
002800*                                 DELETION DATE     (YYYYMMDD)            
002900*** END OF VILMAII-COPY LENGTH= 87 BYTES                                  
