000100 01  RESP-WF0294O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0283         
000300*                                 CURRENCIES YEARLY SCREEN - MAIN         
000400*                                 TENANCE                                 
000500     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000600*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000700*                                 LEGAL SELLER IDENTITY                   
000800     03 RESP-KDVALISO-KEY    PIC X(3).                                    
000900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001000*                                 CURRENCY CODE BY ISO-STANDARD.          
001100     03 RESP-DASTADAT-KEY    PIC 9(8).                                    
001200*                                 GENERELLT STARTDATUM                    
001300*                                 GENERAL START DATE                      
001400     03 RESP-BELEGRAD-1      PIC X(35).                                   
001500*                                 DEL AV LEGAL SELLER NAMN                
001600*                                 PART OF LEGAL SELLER NAME               
001700     03 RESP-REVALUTA-FROM   PIC Z(4)9.                                   
001800*                                 OMRƒKNINGSFAKTOR FR≈N HUVUDVALU         
001900*                                 TA TILL ANDRA VALUTOR                   
002000*                                 RECALCULATION FROM MAIN CURRENC         
002100*                                 Y TO OTHER CURRENCIES                   
002200     03 RESP-REVALUTA-TO     PIC Z(4)9.                                   
002300*                                 OMRƒKNINGSFAKTOR TILL HUVUDVALU         
002400*                                 TA FROM ANDRA VALUTOR                   
002500*                                 RECALCULATION TO MAIN CURRENCY          
002600*                                 FROM OTHER CURRENCIES                   
002700     03 RESP-PRKURS-NEW      PIC Z(5)9.9(6).                              
002800*                                 VALUTAKURS                              
002900*                                 CURRENCY EXCHANGE RATE                  
003000     03 RESP-DAREGDAT        PIC Z(8).                                    
003100*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
003200*                                 REGISTRATION DATE (YYYYMMDD)            
003300     03 RESP-DAUPPDAT        PIC Z(8).                                    
003400*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
003500*                                 UPDATING DATE     (YYYYMMDD)            
003600     03 RESP-DADELDAT        PIC Z(8).                                    
003700*                                 BORTTAGSDATUM      (≈≈≈≈MMDD)           
003800*                                 DELETION DATE     (YYYYMMDD)            
003900     03 RESP-IDUSER          PIC X(8).                                    
004000*                                 ANVƒNDARENS SƒKERHETS ID                
004100*                                 USER SECURITY-IDENTITY                  
004200*** END OF VILMAII-COPY LENGTH= 105 BYTES                                 
