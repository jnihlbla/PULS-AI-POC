000100 01  REQU-WF0294I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0294             
000300*                                 CURRENCY YEARLE SCREEN - MAINTE         
000400*                                 NANCE                                   
000500     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000600*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000700*                                 LEGAL SELLER IDENTITY                   
000800     03 REQU-KDVALISO-KEY    PIC X(3).                                    
000900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001000*                                 CURRENCY CODE BY ISO-STANDARD.          
001100     03 REQU-DASTADAT-KEY    PIC 9(8).                                    
001200*                                 GENERELLT STARTDATUM                    
001300*                                 GENERAL START DATE                      
001400     03 REQU-REVALUTA-FROM   PIC X(5).                                    
001500*                                 OMRÄKNINGSFAKTOR FRÅN HUVUDVALU         
001600*                                 TA TILL ANDRA VALUTOR                   
001700*                                 RECALCULATION FROM MAIN CURRENC         
001800*                                 Y TO OTHER CURRENCIES                   
001900     03 REQU-REVALUTA-TO     PIC X(5).                                    
002000*                                 OMRÄKNINGSFAKTOR TILL HUVUDVALU         
002100*                                 TA FROM ANDRA VALUTOR                   
002200*                                 RECALCULATION TO MAIN CURRENCY          
002300*                                 FROM OTHER CURRENCIES                   
002400     03 REQU-PRKURS-NEW      PIC X(13).                                   
002500*                                 VALUTAKURS                              
002600*                                 CURRENCY EXCHANGE RATE                  
002700*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
