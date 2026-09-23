000100 01  REQU-WF0264I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0264             
000300*                                 DOCUMENT TYPE MAINTENANCE               
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-KDFINDOC-KEY    PIC X(4).                                    
000800*                                 TYP FINANSIELLT DOKUMENT                
000900*                                 FINANCIAL DOCUMENT TYPE                 
001000     03 REQU-KDSTATUS-KEY    PIC 9(3).                                    
001100*                                 STATUSKOD          KDSTATUS-002         
001200     03 REQU-FLCOMING        PIC X.                                       
001300*                                 ALLMƒN FLAGGA                           
001400*                                 GENERAL FLAG                            
001500     03 REQU-FLAP            PIC X.                                       
001600*                                 LEVERANT÷RSRESKONTA                     
001700*                                 ACCOUNTS PAYABLE                        
001800     03 REQU-FLAR            PIC X.                                       
001900*                                 TILL KUNDRESKONTRA                      
002000*                                 ACCONTS RECEIVEABLE                     
002100     03 REQU-FLGL            PIC X.                                       
002200*                                 BOKF÷RINGSTRANSAR                       
002300*                                 GENERAL LEDGER TRANSACTIONS             
002400     03 REQU-FLINTREP        PIC X.                                       
002500*                                 INTRASTATRAPPORTERING                   
002600*                                 CUSTOMS REPORT                          
002700     03 REQU-FLVATREP        PIC X.                                       
002800*                                 MOMSRAPPORT                             
002900*                                 VAT REPORT                              
003000     03 REQU-FLCUSREP        PIC X.                                       
003100*                                 TULLRAPPORT                             
003200*                                 CUSTOMS REPORT                          
003300     03 REQU-BEFINDOC        PIC X(35).                                   
003400*                                 FINANSIELLT DOKUMENT                    
003500*                                 FINANCIAL DOCUMENT                      
003600     03 REQU-DAUPPDAT        PIC X(8).                                    
003700*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
003800*                                                                         
003900*                                 UPDATING DATE     (YYYYMMDD)            
004000*                                                                         
004100*** END OF VILMAII-COPY LENGTH= 61 BYTES                                  
