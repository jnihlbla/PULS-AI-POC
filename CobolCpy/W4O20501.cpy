000100 01  MOD-W4O20501.                                                        
000200*                                 MODCOPYTEXT TILL W40205.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDTRP-IN.                                                     
001000*                                 TRANSPORTIDENTITET                      
001100        05 MOD-IDTRPLOS      PIC X(3).                                    
001200*                                 TRANSPORTLÖSNING                        
001300        05 MOD-IDTRPVAR      PIC X(2).                                    
001400*                                 TRANSPORTLÖSNINGSGRUPP                  
001500     03 MOD-TITRPAVG-IN      PIC X(7).                                    
001600*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
001700     03 MOD-IDDISTR-IN       PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 MOD-KDFRAKT-IN       PIC X(2).                                    
002200*                                 FRAKTSÄTT DC TILL KUND                  
002300     03 MOD-IDORDNR7-IN      PIC X(7).                                    
002400*                                 ORDERNUMMER                             
002500     03 MOD-IDDC-UT          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-IDTRP-UT.                                                     
002800*                                 TRANSPORTIDENTITET                      
002900        05 MOD-IDTRPLOS      PIC X(3).                                    
003000*                                 TRANSPORTLÖSNING                        
003100        05 MOD-IDTRPVAR      PIC X(2).                                    
003200*                                 TRANSPORTLÖSNINGSGRUPP                  
003300     03 MOD-TITRPAVG-UT      PIC X(7).                                    
003400*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
003500     03 MOD-IDDISTR-UT       PIC X(4).                                    
003600*                                 DISTRIKTNUMMER                          
003700     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
003800*                                 KUNDNUMMER                              
003900     03 MOD-KDFRAKT-UT       PIC X(2).                                    
004000*                                 FRAKTSÄTT DC TILL KUND                  
004100     03 MOD-IDORDNR7-UT      PIC X(7).                                    
004200*                                 ORDERNUMMER                             
004300     03 MOD-TIAAMMDD-ENTER   PIC 9(6).                                    
004400*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004500     03 MOD-TIHHMM-ENTER     PIC 9(4).                                    
004600*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
004700     03 MOD-TIAAMMDD-NEXT    PIC 9(6).                                    
004800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004900     03 MOD-TIHHMM-NEXT      PIC 9(4).                                    
005000*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
005100     03 MOD-IDORDER-ENTER    PIC 9(7).                                    
005200*                                 VOLVO PARTS ORDERNUMMER                 
005300     03 MOD-IDORDER-NEXT     PIC 9(7).                                    
005400*                                 VOLVO PARTS ORDERNUMMER                 
005500     03 MOD-IDKUNDNR-ENTER   PIC X(7).                                    
005600*                                 KUNDNUMMER                              
005700     03 MOD-IDKUNDNR-NEXT    PIC X(7).                                    
005800*                                 KUNDNUMMER                              
005900     03 MOD-IDORDNR7-ENTER   PIC 9(7).                                    
006000*                                 ORDERNUMMER                             
006100     03 MOD-IDORDNR7-NEXT    PIC 9(7).                                    
006200*                                 ORDERNUMMER                             
006300     03 MOD-TEDDI            PIC X(9).                                    
006400     03 MOD-RAD              OCCURS 11 TIMES.                             
006500*                                 TABELL-UPDATE                           
006600        05 MOD-CMD-UPDATE-ATTR                                            
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-CMD-UPDATE    PIC X.                                       
007000        05 MOD-IDDISTR-RAD   PIC Z(3)9.                                   
007100*                                 DISTRIKTNUMMER                          
007200        05 MOD-IDKUNDNR-RAD  PIC Z(5)9.                                   
007300*                                 KUNDNUMMER                              
007400        05 MOD-IDORDNR7-RAD  PIC Z(6)9.                                   
007500*                                 ORDERNUMMER                             
007600        05 MOD-KDFRAKT-RAD   PIC Z9.                                      
007700*                                 FRAKTSÄTT DC TILL KUND                  
007800        05 MOD-KDORDKL-RAD   PIC 9.                                       
007900*                                 ORDERKLASS                              
008000        05 MOD-KDTRPKAT-RAD  PIC X.                                       
008100*                                 TRANSPORTKATEGORI                       
008200        05 MOD-VLORDNTO-RAD  PIC Z(2)9.9(3).                              
008300*                                 ORDERVOLYM NETTO (M3)                   
008400        05 MOD-VKORDNTO-RAD  PIC Z(4)9.9.                                 
008500*                                 ORDERVIKT NETTO (KG)                    
008600        05 MOD-SUORDV-RAD    PIC Z(8)9.9(2).                              
008700*                                 SUMMA ORDERVÄRDE                        
008800        05 MOD-TEASTRIX-RAD  PIC X.                                       
008900*                                 ASTERISK                                
009000        05 MOD-IDTRP-RAD.                                                 
009100*                                 TRANSPORTIDENTITET                      
009200           07 MOD-IDTRPLOS   PIC X(3).                                    
009300*                                 TRANSPORTLÖSNING                        
009400           07 MOD-IDTRPVAR   PIC X(2).                                    
009500*                                 TRANSPORTLÖSNINGSGRUPP                  
009600        05 MOD-TITRPAVG-RAD  PIC X(7).                                    
009700*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
009800     03 MOD-VLORDNTO-TOT     PIC Z(3)9.9(3).                              
009900*                                 ORDERVOLYM NETTO (M3)                   
010000     03 MOD-VLORDNTO-TOT-X REDEFINES MOD-VLORDNTO-TOT                     
010100                             PIC X(8).                                    
010200*                                 ORDERVOLYM NETTO (M3)                   
010300     03 MOD-VKORDNTO-TOT     PIC Z(5)9.9.                                 
010400*                                 ORDERVIKT NETTO (KG)                    
010500     03 MOD-VKORDNTO-TOT-X REDEFINES MOD-VKORDNTO-TOT                     
010600                             PIC X(8).                                    
010700*                                 ORDERVIKT NETTO (KG)                    
010800     03 MOD-SUORDV-TOT       PIC Z(8)9.9(2).                              
010900*                                 SUMMA ORDERVÄRDE                        
011000     03 MOD-SUORDV-TOT-X REDEFINES MOD-SUORDV-TOT                         
011100                             PIC X(12).                                   
011200*                                 SUMMA ORDERVÄRDE                        
011300     03 MOD-TEASTRIX-TOT     PIC X.                                       
011400*                                 ASTERISK                                
011500     03 MOD-TEASTRIX-TOT-X REDEFINES MOD-TEASTRIX-TOT                     
011600                             PIC X.                                       
011700*                                 ASTERISK                                
011800     03 MOD-UPDATE.                                                       
011900*                                 TABELL-UPDATE                           
012000        05 MOD-TITRPAVG-UPDATE-ATTR                                       
012100                             PIC X(2).                                    
012200*                                 MFS ATTRIBUTFÄLT                        
012300        05 MOD-TITRPAVG-UPDATE                                            
012400                             PIC X(7).                                    
012500*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
012600        05 MOD-TIRFS-UPDATE-ATTR                                          
012700                             PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900        05 MOD-TIRFS-UPDATE  PIC X(7).                                    
013000*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
013100     03 MOD-IDPRODNR-ENTER   PIC 9(7).                                    
013200*                                 PRODUKTIONSNUMMER                       
013300     03 MOD-IDPRODNR-NEXT    PIC 9(7).                                    
013400*                                 PRODUKTIONSNUMMER                       
013500     03 MOD-IDPLKLST-ENTER   PIC X(3).                                    
013600*                                 PLOCKLISTNUMMER                         
013700     03 MOD-IDPLKLST-NEXT    PIC X(3).                                    
013800*                                 PLOCKLISTNUMMER                         
013900     03 MOD-TEMFSINF         PIC X(55).                                   
014000*                                 INFORMATIONSMEDDELANDE                  
014100*** END OF VILMAII-COPY LENGTH= 996 BYTES                                 
