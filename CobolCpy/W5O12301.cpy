000100 01  MOD-W5O12301.                                                        
000200*                                 MOD CURRENCY INFO                       
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 MOD-KDVALISO-IN      PIC X(3).                                    
001300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001400*                                 CURRENCY CODE BY ISO-STANDARD.          
001500     03 MOD-KDVALTYP-IN      PIC X.                                       
001600*                                 KURSENS PER A=ÅR/M=MÅNAD/D=DAG          
001700*                                 CURRENCY PER YEAR/MONTH/DAY             
001800     03 MOD-IDDC-UT          PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000*                                 WAREHOUSE IDENTIFIER                    
002100     03 MOD-KDVALISO-UT      PIC X(3).                                    
002200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002300*                                 CURRENCY CODE BY ISO-STANDARD.          
002400     03 MOD-KDVALTYP-UT      PIC X.                                       
002500*                                 KURSENS PER A=ÅR/M=MÅNAD/D=DAG          
002600*                                 CURRENCY PER YEAR/MONTH/DAY             
002700     03 MOD-CURRENCYINFO     OCCURS 13 TIMES.                             
002800        05 MOD-KDVALISO      PIC X(3).                                    
002900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003000*                                 CURRENCY CODE BY ISO-STANDARD.          
003100        05 MOD-REVALUTA-TO   PIC Z(4)9.                                   
003200*                                 OMRÄKNINGSFAKTOR TILL HUVUDVALU         
003300*                                 TA FROM ANDRA VALUTOR                   
003400*                                 RECALCULATION TO MAIN CURRENCY          
003500*                                 FROM OTHER CURRENCIES                   
003600        05 MOD-REVALUTA-FROM PIC Z(4)9.                                   
003700*                                 OMRÄKNINGSFAKTOR FRÅN HUVUDVALU         
003800*                                 TA TILL ANDRA VALUTOR                   
003900*                                 RECALCULATION FROM MAIN CURRENC         
004000*                                 Y TO OTHER CURRENCIES                   
004100        05 MOD-PRKURS-NEW    PIC Z(5)9.9(6).                              
004200*                                 VALUTAKURS                              
004300*                                 CURRENCY EXCHANGE RATE                  
004400        05 MOD-TISTADAT      PIC 9(6).                                    
004500*                                 GENERELLT STARTDATUM                    
004600*                                 GENERAL START DATE                      
004700        05 MOD-TIREGDAT      PIC 9(6).                                    
004800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004900*                                 REGISTRATION DATE (YYMMDD)              
005000     03 MOD-TEMFSINF         PIC X(55).                                   
005100*                                 INFORMATIONSMEDDELANDE                  
005200*                                 INFORMATION MESSAGE                     
005300*** END OF VILMAII-COPY LENGTH= 605 BYTES                                 
