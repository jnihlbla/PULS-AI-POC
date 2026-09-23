000100 01  RESP-WF0293O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0293         
000300*                                 CURRENCY YEARLY SCREEN LOCATE           
000400*                                                                         
000500     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000600*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000700*                                 LEGAL SELLER IDENTITY                   
000800     03 RESP-KDVALISO-KEY    PIC X(3).                                    
000900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001000*                                 CURRENCY CODE BY ISO-STANDARD.          
001100     03 RESP-BELEGRAD-1      PIC X(35).                                   
001200*                                 DEL AV LEGAL SELLER NAMN                
001300*                                 PART OF LEGAL SELLER NAME               
001400     03 RESP-KVRADER         PIC Z(4)9.                                   
001500*                                 ANTAL RADER                             
001600*                                 NUMBER OF LINES                         
001700     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
001800*                                 GRUPP MED TABELLRADER                   
001900        05 RESP-KDVALISO-LINE                                             
002000                             PIC X(3).                                    
002100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002200*                                 CURRENCY CODE BY ISO-STANDARD.          
002300        05 RESP-DASTADAT-LINE                                             
002400                             PIC Z(8).                                    
002500*                                 GENERELLT STARTDATUM                    
002600*                                 GENERAL START DATE                      
002700        05 RESP-PRKURS-LINE  PIC Z(5)9.9(6).                              
002800*                                 VALUTAKURS                              
002900*                                 CURRENCY EXCHANGE RATE                  
003000        05 RESP-REVALUTA-FROM-LINE                                        
003100                             PIC Z(4)9.                                   
003200*                                 OMRÄKNINGSFAKTOR FRÅN HUVUDVALU         
003300*                                 TA TILL ANDRA VALUTOR                   
003400*                                 RECALCULATION FROM MAIN CURRENC         
003500*                                 Y TO OTHER CURRENCIES                   
003600        05 RESP-REVALUTA-TO-LINE                                          
003700                             PIC Z(4)9.                                   
003800*                                 OMRÄKNINGSFAKTOR TILL HUVUDVALU         
003900*                                 TA FROM ANDRA VALUTOR                   
004000*                                 RECALCULATION TO MAIN CURRENCY          
004100*                                 FROM OTHER CURRENCIES                   
004200        05 RESP-DAREGDAT-LINE                                             
004300                             PIC Z(8).                                    
004400*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
004500*                                 REGISTRATION DATE (YYYYMMDD)            
004600*** END OF VILMAII-COPY LENGTH= 21047 BYTES                               
