000100 01  MOD-W4O55101.                                                        
000200*                                 MOD-COPYTEXT PGM W40551                 
000300*                                 FRÅGA PÅ TRANSPORTÖR                    
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MOD-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MOD-IDTRP-IN.                                                     
001300*                                 TRANSPORTIDENTITET                      
001400        05 MOD-IDTRPLOS      PIC X(3).                                    
001500*                                 TRANSPORTLÖSNING                        
001600        05 MOD-IDTRPVAR      PIC X(2).                                    
001700*                                 TRANSPORTLÖSNINGSGRUPP                  
001800     03 MOD-IDTRP-UT.                                                     
001900*                                 TRANSPORTIDENTITET                      
002000        05 MOD-IDTRPLOS      PIC X(3).                                    
002100*                                 TRANSPORTLÖSNING                        
002200        05 MOD-IDTRPVAR      PIC X(2).                                    
002300*                                 TRANSPORTLÖSNINGSGRUPP                  
002400     03 MOD-KDODELSTA-IN     PIC X.                                       
002500*                                 ORDERDELSTATUS                          
002600     03 MOD-KDODELSTA-UT     PIC X.                                       
002700*                                 ORDERDELSTATUS                          
002800     03 MOD-TIAAMMDD-ENTER   PIC 9(6).                                    
002900*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003000     03 MOD-TIHHMM-ENTER     PIC 9(4).                                    
003100*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003200     03 MOD-IDORDER-ENTER    PIC 9(7).                                    
003300*                                 VOLVO PARTS ORDERNUMMER                 
003400     03 MOD-TIAAMMDD-NEXT    PIC 9(6).                                    
003500*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003600     03 MOD-TIHHMM-NEXT      PIC 9(4).                                    
003700*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003800     03 MOD-IDORDER-NEXT     PIC 9(7).                                    
003900*                                 VOLVO PARTS ORDERNUMMER                 
004000     03 MOD-FLAGGA-EOF       PIC X.                                       
004100*                                 JA/NEJ-FLAGGA                           
004200     03 MOD-FLAGGA-SCROLL    PIC X.                                       
004300*                                 JA/NEJ-FLAGGA                           
004400     03 MOD-BETRPDST         PIC X(15).                                   
004500*                                 TRANSPORTDESTINATION                    
004600     03 MOD-TITRPAVT         PIC Z(6)9.                                   
004700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004800     03 MOD-BETRPFIR         PIC X(15).                                   
004900*                                 TRANSPORTFIRMANS NAMN                   
005000     03 MOD-TEDDI            PIC X(11).                                   
005100*                                 TEXTFÄLT DDI                            
005200     03 MOD-RAD              OCCURS 13 TIMES.                             
005300*                                 TABELL-RADER                            
005400        05 MOD-IDTRP-ATTR    PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-IDTRP.                                                     
005700*                                 TRANSPORTIDENTITET                      
005800           07 MOD-IDTRPLOS   PIC X(3).                                    
005900*                                 TRANSPORTLÖSNING                        
006000           07 MOD-IDTRPVAR   PIC X(2).                                    
006100*                                 TRANSPORTLÖSNINGSGRUPP                  
006200     03 MOD-RAD              OCCURS 13 TIMES.                             
006300*                                 TABELL-RADER                            
006400        05 MOD-IDDISTR-ATTR  PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-IDDISTR       PIC Z(3)9.                                   
006700*                                 DISTRIKTNUMMER                          
006800     03 MOD-RAD              OCCURS 13 TIMES.                             
006900*                                 TABELL-RADER                            
007000        05 MOD-IDKUNDNR-ATTR PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
007300*                                 KUNDNUMMER                              
007400     03 MOD-RAD              OCCURS 13 TIMES.                             
007500*                                 TABELL-RADER                            
007600        05 MOD-IDKUNDRF-ATTR PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-IDKUNDRF      PIC Z(6)9.                                   
007900*                                 ORDERNUMMER                             
008000     03 MOD-RAD              OCCURS 13 TIMES.                             
008100*                                 TABELL-RADER                            
008200        05 MOD-STATUS-ATTR   PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-STATUS        PIC X(2).                                    
008500     03 MOD-RAD              OCCURS 13 TIMES.                             
008600*                                 TABELL-RADER                            
008700        05 MOD-VLORDNTO-ATTR PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900        05 MOD-VLORDNTO      PIC Z(3)9.9(3).                              
009000*                                 ORDERVOLYM NETTO (M3)                   
009100     03 MOD-RAD              OCCURS 13 TIMES.                             
009200*                                 TABELL-RADER                            
009300        05 MOD-VKORDNTO-ATTR PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500        05 MOD-VKORDNTO      PIC Z(5)9.9.                                 
009600*                                 ORDERVIKT NETTO (KG)                    
009700     03 MOD-RAD              OCCURS 13 TIMES.                             
009800*                                 TABELL-RADER                            
009900        05 MOD-SUORDV-ATTR   PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100        05 MOD-SUORDV        PIC Z(8)9.9(2).                              
010200*                                 SUMMA ORDERVÄRDE                        
010300        05 MOD-TEASTRIX-RAD  PIC X.                                       
010400*                                 ASTERISK                                
010500     03 MOD-VLORDNTO-TOTAL   PIC Z(3)9.9(3).                              
010600*                                 ORDERVOLYM NETTO (M3)                   
010700     03 MOD-VKORDNTO-TOTAL   PIC Z(5)9.9.                                 
010800*                                 ORDERVIKT NETTO (KG)                    
010900     03 MOD-SUORDV-TOTAL     PIC Z(8)9.9(2).                              
011000*                                 SUMMA ORDERVÄRDE                        
011100     03 MOD-TEASTRIX-ORD     PIC X.                                       
011200*                                 ASTERISK                                
011300     03 MOD-VLORDNTO-SPAR    PIC 9(4)V9(3).                               
011400*                                 ORDERVOLYM BRUTTO (M3)                  
011500     03 MOD-VKORDNTO-SPAR    PIC 9(6)V9(1).                               
011600*                                 ORDERVIKT BRUTTO (KG)                   
011700     03 MOD-SUORDV-SPAR      PIC 9(9)V9(2).                               
011800     03 MOD-TEMFSINF         PIC X(55).                                   
011900*                                 INFORMATIONSMEDDELANDE                  
012000*** END OF VILMAII-COPY LENGTH= 1150 BYTES                                
