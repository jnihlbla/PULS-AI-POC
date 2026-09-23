000100 01  REQU-WF0274I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0274             
000300*                                 BUSINESS RELATIONS MAINTENANCE          
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-KDFINDOC-KEY    PIC X(4).                                    
000800*                                 TYP FINANSIELLT DOKUMENT                
000900*                                 FINANCIAL DOCUMENT TYPE                 
001000     03 REQU-KDPARTTY-KEY    PIC X(3).                                    
001100*                                 TYP AV BETALARE                         
001200*                                 TYPE OF FIN.CUSTOMER                    
001300     03 REQU-KDPARTGR-KEY    PIC X(15).                                   
001400*                                 GRUPP AV BETALARE                       
001500*                                 FIN.CUSTOMER GROUP                      
001600     03 REQU-KDSTATUS-KEY    PIC 9(3).                                    
001700*                                 STATUSKOD          KDSTATUS-002         
001800     03 REQU-FLCOMING        PIC X.                                       
001900*                                 ALLMÄN FLAGGA                           
002000*                                 GENERAL FLAG                            
002100     03 REQU-BEFORMS         PIC X(15).                                   
002200*                                 BENÄMNING PÅ DOKUMENTFORMAT             
002300*                                                                         
002400*                                 DESCRIPTION OF DOCUMENT FORMAT          
002500*                                                                         
002600     03 REQU-FLGL            PIC X.                                       
002700*                                 BOKFÖRINGSTRANSAR                       
002800*                                 GENERAL LEDGER TRANSACTIONS             
002900     03 REQU-FLAR            PIC X.                                       
003000*                                 TILL KUNDRESKONTRA                      
003100*                                 ACCONTS RECEIVEABLE                     
003200     03 REQU-FLAP            PIC X.                                       
003300*                                 LEVERANTÖRSRESKONTA                     
003400*                                 ACCOUNTS PAYABLE                        
003500     03 REQU-FLVATCHK        PIC X.                                       
003600*                                 MOMSKONTROLL                            
003700*                                 VAT CONTROL                             
003800     03 REQU-DAUPPDAT        PIC X(8).                                    
003900*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
004000*                                                                         
004100*                                 UPDATING DATE     (YYYYMMDD)            
004200*                                                                         
004300*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  
