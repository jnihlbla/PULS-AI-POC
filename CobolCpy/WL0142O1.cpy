000100 01  RESP-WL0142O1.                                                       
000200*                                 RESPONS FROM PGM WL0142                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
000800*                                 KUNDNUMMER                              
000900     03 RESP-IDORDNR7-KEY    PIC Z(6)9.                                   
001000*                                 ORDERNUMMER                             
001100     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
001200*                                 ARTIKELNUMMER                           
001300     03 RESP-IDKOLLI-KEY     PIC Z(4)9.                                   
001400*                                 KOLLINUMMER                             
001500     03 RESP-IDPRODNR-KEY    PIC Z(6)9.                                   
001600*                                 PRODUKTIONSNUMMER                       
001700     03 RESP-TEDDI           PIC X(11).                                   
001800*                                 TEXTFÄLT DDI                            
001900     03 RESP-BEKUNDRF        PIC X(15).                                   
002000*                                 KUNDENS REFERENS                        
002100     03 RESP-IDKAMPRF        PIC Z(6)9.                                   
002200*                                 KAMPANJREFERENS                         
002300     03 RESP-TIREGDAT        PIC 9(6).                                    
002400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002500     03 RESP-TIHHMM-REG      PIC 9(4).                                    
002600*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
002700     03 RESP-TIAAMMDD-RFS    PIC 9(6).                                    
002800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002900     03 RESP-IDTRP.                                                       
003000*                                 TRANSPORTIDENTITET                      
003100        05 RESP-IDTRPLOS     PIC X(3).                                    
003200*                                 TRANSPORTLÖSNING                        
003300        05 RESP-IDTRPVAR     PIC X(2).                                    
003400*                                 TRANSPORTLÖSNINGSGRUPP                  
003500     03 RESP-TIAAMMDD-TRP    PIC 9(6).                                    
003600*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003700     03 RESP-TIHHMM-TRP      PIC 9(4).                                    
003800*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003900     03 RESP-TIREGDAT-STO    PIC 9(6).                                    
004000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004100     03 RESP-TIHHMM-REG-STO  PIC 9(4).                                    
004200*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
004300     03 RESP-TIREPDAT        PIC 9(6).                                    
004400*                                 REPAIR DATE                             
004500     03 RESP-SUORDV-TOT      PIC Z(8)9.9(2).                              
004600*                                 SUMMA ORDERVÄRDE                        
004700     03 RESP-ASTERIX1        PIC X.                                       
004800*                                 ASTERISK                                
004900     03 RESP-KVRADER-TOT     PIC Z(4)9.                                   
005000*                                 ANTAL RADER                             
005100     03 RESP-SUORDV-U        PIC Z(8)9.9(2).                              
005200*                                 SUMMA ORDERVÄRDE                        
005300     03 RESP-ASTERIX2        PIC X.                                       
005400*                                 ASTERISK                                
005500     03 RESP-SUORDV-P        PIC Z(8)9.9(2).                              
005600*                                 SUMMA ORDERVÄRDE                        
005700     03 RESP-ASTERIX3        PIC X.                                       
005800*                                 ASTERISK                                
005900     03 RESP-KVRADER-P       PIC Z(4)9.                                   
006000*                                 ANTAL RADER                             
006100     03 RESP-SUORDV-F        PIC Z(8)9.9(2).                              
006200*                                 SUMMA ORDERVÄRDE                        
006300     03 RESP-ASTERIX4        PIC X.                                       
006400*                                 ASTERISK                                
006500     03 RESP-KVRADER-F       PIC Z(4)9.                                   
006600*                                 ANTAL RADER                             
006700     03 RESP-SUORDV-L        PIC Z(8)9.9(2).                              
006800*                                 SUMMA ORDERVÄRDE                        
006900     03 RESP-ASTERIX5        PIC X.                                       
007000*                                 ASTERISK                                
007100     03 RESP-KVRADER-L       PIC Z(4)9.                                   
007200*                                 ANTAL RADER                             
007300     03 RESP-FAKTURA-GRP     OCCURS 12 TIMES.                             
007400*                                 FAKTURANUMMER OCH FAKTURADATUM          
007500        05 RESP-IDFAKT       PIC Z(6)9.                                   
007600*                                 FAKTURANUMMER                           
007700        05 RESP-TIFAKT       PIC 9(6).                                    
007800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
007900*** END OF VILMAII-COPY LENGTH= 360 BYTES                                 
