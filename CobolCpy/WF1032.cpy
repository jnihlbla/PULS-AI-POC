000100 01  WF1032.                                                              
000200*                                 T01CUYE DATA                            
000300     03 IDLEGSEL             PIC X(4).                                    
000400*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000500*                                 LEGAL SELLER IDENTITY                   
000600     03 KDVALISO             PIC X(5).                                    
000700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
000800*                                 CURRENCY CODE BY ISO-STANDARD.          
000900     03 DASTADAT             PIC 9(8).                                    
001000*                                 GENERELLT STARTDATUM                    
001100*                                 GENERAL START DATE                      
001200     03 REVALUTA-FROM        PIC S9(5)           COMP-3.                  
001300*                                 OMRƒKNINGSFAKTOR FR≈N HUVUDVALU         
001400*                                 TA TILL ANDRA VALUTOR                   
001500*                                 RECALCULATION FROM MAIN CURRENC         
001600*                                 Y TO OTHER CURRENCIES                   
001700     03 REVALUTA-TO          PIC S9(5)           COMP-3.                  
001800*                                 OMRƒKNINGSFAKTOR TILL HUVUDVALU         
001900*                                 TA FROM ANDRA VALUTOR                   
002000*                                 RECALCULATION TO MAIN CURRENCY          
002100*                                 FROM OTHER CURRENCIES                   
002200     03 PRKURS-NEW           PIC S9(6)V9(6)      COMP-3.                  
002300*                                 VALUTAKURS                              
002400*                                 CURRENCY EXCHANGE RATE                  
002500     03 DAREGDAT             PIC 9(8).                                    
002600*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
002700*                                 REGISTRATION DATE (YYYYMMDD)            
002800     03 DAUPPDAT             PIC 9(8).                                    
002900*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
003000*                                 UPDATING DATE     (YYYYMMDD)            
003100     03 DADELDAT             PIC 9(8).                                    
003200*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
003300*                                 DELETION DATE     (YYYYMMDD)            
003400     03 IDUSER               PIC X(8).                                    
003500*                                 ANVƒNDARENS SƒKERHETS ID                
003600*                                 USER SECURITY-IDENTITY                  
003700*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
