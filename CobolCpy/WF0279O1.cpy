000100 01  RESP-WF0279O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0279         
000300*                                 VAT MAINTENANCE                         
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-IDLANDX2-KEY    PIC X(2).                                    
000800*                                 2-STƒLLIG LANDSBETECKNINGSKOD           
000900*                                 2-LETTER CODE FOR COUNTRY               
001000     03 RESP-KDVAT-KEY       PIC X(2).                                    
001100*                                 MOMSKOD                                 
001200*                                 VAT CODE                                
001300     03 RESP-BELEGRAD-1      PIC X(35).                                   
001400*                                 DEL AV LEGAL SELLER NAMN                
001500*                                 PART OF LEGAL SELLER NAME               
001600     03 RESP-BEVAT           PIC X(50).                                   
001700*                                 MOMSKODSBENƒMNING R3                    
001800*                                 VAT CODE DESCRIPTION R3                 
001900     03 RESP-REVAT           PIC X(6).                                    
002000*                                 MULTIPLIKATIONSFAKTOR F÷R MOMS          
002100*                                 VAT FACTOR                              
002200     03 RESP-DAREGDAT        PIC Z(8).                                    
002300*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
002400*                                 REGISTRATION DATE (YYYYMMDD)            
002500     03 RESP-DAUPPDAT        PIC Z(8).                                    
002600*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
002700*                                                                         
002800*                                 UPDATING DATE     (YYYYMMDD)            
002900*                                                                         
003000     03 RESP-DADELDAT        PIC Z(8).                                    
003100*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
003200*                                 DELETION DATE     (YYYYMMDD)            
003300     03 RESP-IDUSER          PIC X(8).                                    
003400*                                 ANVƒNDARENS SƒKERHETS ID                
003500*                                 USER SECURITY-IDENTITY                  
003600*** END OF VILMAII-COPY LENGTH= 131 BYTES                                 
