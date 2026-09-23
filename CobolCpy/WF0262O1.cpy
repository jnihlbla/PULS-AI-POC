000100 01  RESP-WF0262O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0262         
000300*                                 CUSTOMER GROUP MAINTENANCE              
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-KDPARTTY-KEY    PIC X(3).                                    
000800*                                 TYP AV BETALARE                         
000900*                                 TYPE OF FIN.CUSTOMER                    
001000     03 RESP-KDPARTGR-KEY    PIC X(15).                                   
001100*                                 GRUPP AV BETALARE                       
001200*                                 FIN.CUSTOMER GROUP                      
001300     03 RESP-KDSTATUS-KEY    PIC 9(3).                                    
001400*                                 STATUSKOD          KDSTATUS-002         
001500     03 RESP-BELEGRAD-1      PIC X(35).                                   
001600*                                 DEL AV LEGAL SELLER NAMN                
001700*                                 PART OF LEGAL SELLER NAME               
001800     03 RESP-FLCOMING        PIC X.                                       
001900*                                 ALLMƒN FLAGGA                           
002000*                                 GENERAL FLAG                            
002100     03 RESP-KDINVFRQ        PIC X(4).                                    
002200*                                 FAKTURERINGSFREKVENS                    
002300*                                 INVOICE FREQUENCE                       
002400     03 RESP-KDAPPEND        PIC X(4).                                    
002500*                                 APPENDIX KOD                            
002600*                                 APPENDIX CODE                           
002700     03 RESP-FLVAT           PIC X.                                       
002800*                                 MOMS P≈ FAKTURA                         
002900*                                 TAX ON INVOICE                          
003000     03 RESP-DAREGDAT        PIC Z(8).                                    
003100*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
003200*                                 REGISTRATION DATE (YYYYMMDD)            
003300     03 RESP-DAUPPDAT        PIC Z(8).                                    
003400*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
003500*                                                                         
003600*                                 UPDATING DATE     (YYYYMMDD)            
003700*                                                                         
003800     03 RESP-DADELDAT        PIC Z(8).                                    
003900*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
004000*                                 DELETION DATE     (YYYYMMDD)            
004100     03 RESP-IDUSER          PIC X(8).                                    
004200*                                 ANVƒNDARENS SƒKERHETS ID                
004300*                                 USER SECURITY-IDENTITY                  
004400*** END OF VILMAII-COPY LENGTH= 102 BYTES                                 
