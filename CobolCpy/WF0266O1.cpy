000100 01  RESP-WF0266O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0266         
000300*                                 SENDING COUNTRY MAINTENANCE             
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-IDLANDX3-KEY    PIC X(3).                                    
000800*                                 3-STƒLLIG LANDSBETECKNINGSKOD           
000900*                                 3-LETTER CODE FOR COUNTRY.              
001000     03 RESP-KDSTATUS-KEY    PIC 9(3).                                    
001100*                                 STATUSKOD          KDSTATUS-002         
001200     03 RESP-BELEGRAD-1      PIC X(35).                                   
001300*                                 DEL AV LEGAL SELLER NAMN                
001400*                                 PART OF LEGAL SELLER NAME               
001500     03 RESP-FLCOMING        PIC X.                                       
001600*                                 ALLMƒN FLAGGA                           
001700*                                 GENERAL FLAG                            
001800     03 RESP-BELAND          PIC X(35).                                   
001900*                                 LANDSBETECKNING                         
002000*                                 NAME OF COUNTRY                         
002100     03 RESP-IDVAT           PIC X(17).                                   
002200*                                 MOMSREGISTRERINGSNUMMER                 
002300*                                 VAT REGISTRATION NUMBER                 
002400     03 RESP-BETEXT-1        PIC X(50).                                   
002500     03 RESP-BETEXT-2        PIC X(50).                                   
002600     03 RESP-BETEXT-3        PIC X(50).                                   
002700     03 RESP-BETEXT-4        PIC X(50).                                   
002800     03 RESP-KDVALISO        PIC X(3).                                    
002900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003000*                                 CURRENCY CODE BY ISO-STANDARD.          
003100     03 RESP-DAREGDAT        PIC Z(8).                                    
003200*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
003300*                                 REGISTRATION DATE (YYYYMMDD)            
003400     03 RESP-DAUPPDAT        PIC Z(8).                                    
003500*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
003600*                                                                         
003700*                                 UPDATING DATE     (YYYYMMDD)            
003800*                                                                         
003900     03 RESP-DADELDAT        PIC Z(8).                                    
004000*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
004100*                                 DELETION DATE     (YYYYMMDD)            
004200     03 RESP-IDUSER          PIC X(8).                                    
004300*                                 ANVƒNDARENS SƒKERHETS ID                
004400*                                 USER SECURITY-IDENTITY                  
004500*** END OF VILMAII-COPY LENGTH= 333 BYTES                                 
