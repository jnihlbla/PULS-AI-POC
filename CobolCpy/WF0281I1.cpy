000100 01  REQU-WF0281I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0281             
000300*                                 PAYMENT INSTRUCTIONS MAINTENANC         
000400*                                 E                                       
000500     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000600*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000700*                                 LEGAL SELLER IDENTITY                   
000800     03 REQU-KDFINDOC-KEY    PIC X(4).                                    
000900*                                 TYP FINANSIELLT DOKUMENT                
001000*                                 FINANCIAL DOCUMENT TYPE                 
001100     03 REQU-KDVALISO-KEY    PIC X(3).                                    
001200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001300*                                 CURRENCY CODE BY ISO-STANDARD.          
001400     03 REQU-KDPARTTY-KEY    PIC X(3).                                    
001500*                                 TYP AV BETALARE                         
001600*                                 TYPE OF FIN.CUSTOMER                    
001700     03 REQU-KDPARTGR-KEY    PIC X(15).                                   
001800*                                 GRUPP AV BETALARE                       
001900*                                 FIN.CUSTOMER GROUP                      
002000     03 REQU-KDSTATUS-KEY    PIC 9(3).                                    
002100*                                 STATUSKOD          KDSTATUS-002         
002200     03 REQU-FLCOMING        PIC X.                                       
002300*                                 ALLMƒN FLAGGA                           
002400*                                 GENERAL FLAG                            
002500     03 REQU-BETEXT-1        PIC X(50).                                   
002600     03 REQU-BETEXT-2        PIC X(50).                                   
002700     03 REQU-BETEXT-3        PIC X(50).                                   
002800     03 REQU-BETEXT-4        PIC X(50).                                   
002900     03 REQU-DAUPPDAT        PIC X(8).                                    
003000*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
003100*                                                                         
003200*                                 UPDATING DATE     (YYYYMMDD)            
003300*                                                                         
003400*** END OF VILMAII-COPY LENGTH= 241 BYTES                                 
