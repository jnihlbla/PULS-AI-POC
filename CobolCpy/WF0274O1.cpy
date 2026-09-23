000100 01  RESP-WF0274O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0274         
000300*                                 BUSINESS RELATIONS MAINTENANCE          
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-KDFINDOC-KEY    PIC X(4).                                    
000800*                                 TYP FINANSIELLT DOKUMENT                
000900*                                 FINANCIAL DOCUMENT TYPE                 
001000     03 RESP-KDPARTTY-KEY    PIC X(3).                                    
001100*                                 TYP AV BETALARE                         
001200*                                 TYPE OF FIN.CUSTOMER                    
001300     03 RESP-KDPARTGR-KEY    PIC X(15).                                   
001400*                                 GRUPP AV BETALARE                       
001500*                                 FIN.CUSTOMER GROUP                      
001600     03 RESP-KDSTATUS-KEY    PIC 9(3).                                    
001700*                                 STATUSKOD          KDSTATUS-002         
001800     03 RESP-BELEGRAD-1      PIC X(35).                                   
001900*                                 DEL AV LEGAL SELLER NAMN                
002000*                                 PART OF LEGAL SELLER NAME               
002100     03 RESP-FLCOMING        PIC X.                                       
002200*                                 ALLMƒN FLAGGA                           
002300*                                 GENERAL FLAG                            
002400     03 RESP-BEFORMS         PIC X(15).                                   
002500*                                 BENƒMNING P≈ DOKUMENTFORMAT             
002600*                                                                         
002700*                                 DESCRIPTION OF DOCUMENT FORMAT          
002800*                                                                         
002900     03 RESP-FLGL            PIC X.                                       
003000*                                 BOKF÷RINGSTRANSAR                       
003100*                                 GENERAL LEDGER TRANSACTIONS             
003200     03 RESP-FLAR            PIC X.                                       
003300*                                 TILL KUNDRESKONTRA                      
003400*                                 ACCONTS RECEIVEABLE                     
003500     03 RESP-FLAP            PIC X.                                       
003600*                                 LEVERANT÷RSRESKONTA                     
003700*                                 ACCOUNTS PAYABLE                        
003800     03 RESP-FLVATCHK        PIC X.                                       
003900*                                 MOMSKONTROLL                            
004000*                                 VAT CONTROL                             
004100     03 RESP-DAREGDAT        PIC Z(8).                                    
004200*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
004300*                                 REGISTRATION DATE (YYYYMMDD)            
004400     03 RESP-DAUPPDAT        PIC Z(8).                                    
004500*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
004600*                                                                         
004700*                                 UPDATING DATE     (YYYYMMDD)            
004800*                                                                         
004900     03 RESP-DADELDAT        PIC Z(8).                                    
005000*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
005100*                                 DELETION DATE     (YYYYMMDD)            
005200     03 RESP-IDUSER          PIC X(8).                                    
005300*                                 ANVƒNDARENS SƒKERHETS ID                
005400*                                 USER SECURITY-IDENTITY                  
005500*** END OF VILMAII-COPY LENGTH= 116 BYTES                                 
