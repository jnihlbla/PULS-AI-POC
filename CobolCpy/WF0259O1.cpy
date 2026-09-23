000100 01  RESP-WF0259O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0259         
000300*                                 CURRENCY LOCATE                         
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-KDVALISO-KEY    PIC X(3).                                    
000800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
000900*                                 CURRENCY CODE BY ISO-STANDARD.          
001000     03 RESP-BELEGRAD-1      PIC X(35).                                   
001100*                                 DEL AV LEGAL SELLER NAMN                
001200*                                 PART OF LEGAL SELLER NAME               
001300     03 RESP-KVRADER         PIC Z(4)9.                                   
001400*                                 ANTAL RADER                             
001500*                                 NUMBER OF LINES                         
001600     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
001700*                                 GRUPP MED TABELLRADER                   
001800        05 RESP-KDVALISO-LINE                                             
001900                             PIC X(3).                                    
002000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002100*                                 CURRENCY CODE BY ISO-STANDARD.          
002200        05 RESP-DASTADAT-LINE                                             
002300                             PIC Z(8).                                    
002400*                                 GENERELLT STARTDATUM                    
002500*                                 GENERAL START DATE                      
002600        05 RESP-PRKURS-LINE  PIC Z(5)9.9(5).                              
002700*                                 VALUTAKURS                              
002800*                                 CURRENCY EXCHANGE RATE                  
002900        05 RESP-REVALUTA-LINE                                             
003000                             PIC Z(4)9.                                   
003100*                                 OMRƒKNINGSTAL F÷R VALUTA                
003200*                                 CONVERT VALUE FOR CURRENCY CODE         
003300        05 RESP-DAREGDAT-LINE                                             
003400                             PIC Z(8).                                    
003500*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
003600*                                 REGISTRATION DATE (YYYYMMDD)            
003700*** END OF VILMAII-COPY LENGTH= 18047 BYTES                               
