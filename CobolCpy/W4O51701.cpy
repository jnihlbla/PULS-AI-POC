000100 01  MOD-W4O51701.                                                        
000200*                                 COPYTEXT FÖR MOD W4O51701               
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-IDDC-IN          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MOD-IDDC-UT          PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 MOD-KDORDKL-IN       PIC X.                                       
002100*                                 ORDERKLASS                              
002200     03 MOD-KDORDKL-UT       PIC X.                                       
002300*                                 ORDERKLASS                              
002400     03 MOD-KDFRAKT-IN       PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-KDFRAKT-UT       PIC X(2).                                    
002700*                                 FRAKTSÄTT C1-C2 TILL KUND               
002800     03 MOD-KDORDSTA-IN      PIC X.                                       
002900*                                 VOLVOORDERSTATUS                        
003000     03 MOD-KDORDSTA-UT      PIC X.                                       
003100*                                 VOLVOORDERSTATUS                        
003200     03 MOD-IDKUNDNR-ENTER   PIC 9(6).                                    
003300*                                 KUNDNUMMER                              
003400     03 MOD-IDKUNDNR-NEXT    PIC 9(6).                                    
003500*                                 KUNDNUMMER                              
003600     03 MOD-IDPRODNR-ENTER   PIC 9(7).                                    
003700*                                 PRODUKTIONSNUMMER                       
003800     03 MOD-IDPRODNR-NEXT    PIC 9(7).                                    
003900*                                 PRODUKTIONSNUMMER                       
004000     03 MOD-IDKUNDRF-ENTER   PIC X(10).                                   
004100*                                 KUNDENS REFERENS (ORDERID)              
004200     03 MOD-IDKUNDRF-NEXT    PIC X(10).                                   
004300*                                 KUNDENS REFERENS (ORDERID)              
004400     03 MOD-UTDATA-RAD       OCCURS 14 TIMES.                             
004500        05 MOD-IDTRANS-RAD   PIC X(4).                                    
004600*                                 BILDNUMMER                              
004700        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
004800*                                 KUNDNUMMER                              
004900        05 MOD-IDORDNR7      PIC Z(6)9.                                   
005000*                                 ORDERNUMMER                             
005100        05 MOD-IDDC          PIC X(2).                                    
005200*                                 IDENTIFIERARE LAGER                     
005300        05 MOD-KDORDKL       PIC X.                                       
005400*                                 ORDERKLASS                              
005500        05 MOD-TIREGDAT      PIC 9(6).                                    
005600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005700        05 MOD-TIBEGPAC      PIC 9(6).                                    
005800*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
005900        05 MOD-TIPACKN-SK    PIC 9(6).                                    
006000*                                 PACKNINGSDATUM SENASTE KOLLI            
006100        05 MOD-TIFAKT-SK     PIC 9(6).                                    
006200*                                 FAKTURADATUM SENASTE KOLLI              
006300        05 MOD-TIAAMMDD      PIC 9(6).                                    
006400*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
006500        05 MOD-TIHHMM        PIC 9(4).                                    
006600*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
006700        05 MOD-TILASTN-SK    PIC 9(6).                                    
006800*                                 LASTNINGSDATUM SENASTE KOLLI            
006900        05 MOD-KVARBDAG      PIC Z(2)9.                                   
007000*                                 ANTAL ARBETSDAGAR                       
007100     03 MOD-TEMFSINF         PIC X(55).                                   
007200*                                 INFORMATIONSMEDDELANDE                  
007300*** END OF VILMAII-COPY LENGTH= 1053 BYTES                                
