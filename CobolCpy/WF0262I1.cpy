000100 01  REQU-WF0262I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0262             
000300*                                 CUSTOMER GROUP MAINTENANCE              
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-KDPARTTY-KEY    PIC X(3).                                    
000800*                                 TYP AV BETALARE                         
000900*                                 TYPE OF FIN.CUSTOMER                    
001000     03 REQU-KDPARTGR-KEY    PIC X(15).                                   
001100*                                 GRUPP AV BETALARE                       
001200*                                 FIN.CUSTOMER GROUP                      
001300     03 REQU-KDSTATUS-KEY    PIC 9(3).                                    
001400*                                 STATUSKOD          KDSTATUS-002         
001500     03 REQU-FLCOMING        PIC X.                                       
001600*                                 ALLMƒN FLAGGA                           
001700*                                 GENERAL FLAG                            
001800     03 REQU-KDINVFRQ        PIC X(4).                                    
001900*                                 FAKTURERINGSFREKVENS                    
002000*                                 INVOICE FREQUENCE                       
002100     03 REQU-KDAPPEND        PIC X(4).                                    
002200*                                 APPENDIX KOD                            
002300*                                 APPENDIX CODE                           
002400     03 REQU-FLVAT           PIC X.                                       
002500*                                 MOMS P≈ FAKTURA                         
002600*                                 TAX ON INVOICE                          
002700     03 REQU-DAUPPDAT        PIC X(8).                                    
002800*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
002900*                                                                         
003000*                                 UPDATING DATE     (YYYYMMDD)            
003100*                                                                         
003200*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  
