000100 01  RESP-WF0264O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0264         
000300*                                 DOCUMENT TYPE MAINTENANCE               
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-KDFINDOC-KEY    PIC X(4).                                    
000800*                                 TYP FINANSIELLT DOKUMENT                
000900*                                 FINANCIAL DOCUMENT TYPE                 
001000     03 RESP-KDSTATUS-KEY    PIC 9(3).                                    
001100*                                 STATUSKOD          KDSTATUS-002         
001200     03 RESP-BELEGRAD-1      PIC X(35).                                   
001300*                                 DEL AV LEGAL SELLER NAMN                
001400*                                 PART OF LEGAL SELLER NAME               
001500     03 RESP-FLCOMING        PIC X.                                       
001600*                                 ALLMƒN FLAGGA                           
001700*                                 GENERAL FLAG                            
001800     03 RESP-FLAP            PIC X.                                       
001900*                                 LEVERANT÷RSRESKONTA                     
002000*                                 ACCOUNTS PAYABLE                        
002100     03 RESP-FLAR            PIC X.                                       
002200*                                 TILL KUNDRESKONTRA                      
002300*                                 ACCONTS RECEIVEABLE                     
002400     03 RESP-FLGL            PIC X.                                       
002500*                                 BOKF÷RINGSTRANSAR                       
002600*                                 GENERAL LEDGER TRANSACTIONS             
002700     03 RESP-FLINTREP        PIC X.                                       
002800*                                 INTRASTATRAPPORTERING                   
002900*                                 CUSTOMS REPORT                          
003000     03 RESP-FLVATREP        PIC X.                                       
003100*                                 MOMSRAPPORT                             
003200*                                 VAT REPORT                              
003300     03 RESP-FLCUSREP        PIC X.                                       
003400*                                 TULLRAPPORT                             
003500*                                 CUSTOMS REPORT                          
003600     03 RESP-BEFINDOC        PIC X(35).                                   
003700*                                 FINANSIELLT DOKUMENT                    
003800*                                 FINANCIAL DOCUMENT                      
003900     03 RESP-DAREGDAT        PIC Z(8).                                    
004000*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
004100*                                 REGISTRATION DATE (YYYYMMDD)            
004200     03 RESP-DAUPPDAT        PIC Z(8).                                    
004300*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
004400*                                                                         
004500*                                 UPDATING DATE     (YYYYMMDD)            
004600*                                                                         
004700     03 RESP-DADELDAT        PIC Z(8).                                    
004800*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
004900*                                 DELETION DATE     (YYYYMMDD)            
005000     03 RESP-IDUSER          PIC X(8).                                    
005100*                                 ANVƒNDARENS SƒKERHETS ID                
005200*                                 USER SECURITY-IDENTITY                  
005300*** END OF VILMAII-COPY LENGTH= 120 BYTES                                 
