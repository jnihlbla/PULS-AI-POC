000100 01  RESP-WF0281O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0281         
000300*                                 PAYMENT INSTRUCTIONS MAINTENANC         
000400*                                 E                                       
000500     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000600*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000700*                                 LEGAL SELLER IDENTITY                   
000800     03 RESP-KDFINDOC-KEY    PIC X(4).                                    
000900*                                 TYP FINANSIELLT DOKUMENT                
001000*                                 FINANCIAL DOCUMENT TYPE                 
001100     03 RESP-KDVALISO-KEY    PIC X(3).                                    
001200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001300*                                 CURRENCY CODE BY ISO-STANDARD.          
001400     03 RESP-KDPARTTY-KEY    PIC X(3).                                    
001500*                                 TYP AV BETALARE                         
001600*                                 TYPE OF FIN.CUSTOMER                    
001700     03 RESP-KDPARTGR-KEY    PIC X(15).                                   
001800*                                 GRUPP AV BETALARE                       
001900*                                 FIN.CUSTOMER GROUP                      
002000     03 RESP-KDSTATUS-KEY    PIC 9(3).                                    
002100*                                 STATUSKOD          KDSTATUS-002         
002200     03 RESP-BELEGRAD-1      PIC X(35).                                   
002300*                                 DEL AV LEGAL SELLER NAMN                
002400*                                 PART OF LEGAL SELLER NAME               
002500     03 RESP-FLCOMING        PIC X.                                       
002600*                                 ALLMƒN FLAGGA                           
002700*                                 GENERAL FLAG                            
002800     03 RESP-BETEXT-1        PIC X(50).                                   
002900     03 RESP-BETEXT-2        PIC X(50).                                   
003000     03 RESP-BETEXT-3        PIC X(50).                                   
003100     03 RESP-BETEXT-4        PIC X(50).                                   
003200     03 RESP-DAREGDAT        PIC Z(8).                                    
003300*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
003400*                                 REGISTRATION DATE (YYYYMMDD)            
003500     03 RESP-DAUPPDAT        PIC Z(8).                                    
003600*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
003700*                                                                         
003800*                                 UPDATING DATE     (YYYYMMDD)            
003900*                                                                         
004000     03 RESP-DADELDAT        PIC Z(8).                                    
004100*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
004200*                                 DELETION DATE     (YYYYMMDD)            
004300     03 RESP-IDUSER          PIC X(8).                                    
004400*                                 ANVƒNDARENS SƒKERHETS ID                
004500*                                 USER SECURITY-IDENTITY                  
004600*** END OF VILMAII-COPY LENGTH= 300 BYTES                                 
