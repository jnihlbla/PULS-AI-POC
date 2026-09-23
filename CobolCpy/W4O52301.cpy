000100 01  MOD-W4O52301.                                                        
000200*                                 MOD-COPYTEXT FÖR W40523                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MOD-IDORDNR7-IN      PIC X(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 MOD-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001600*                                 KUNDNUMMER                              
001700     03 MOD-IDORDNR7-UT      PIC X(7).                                    
001800*                                 ORDERNUMMER                             
001900     03 MOD-FLFORTS          PIC X.                                       
002000     03 MOD-IDDISTR-KY       PIC 9(4).                                    
002100*                                 DISTRIKTNUMMER                          
002200     03 MOD-IDKUNDNR-KY      PIC 9(6).                                    
002300*                                 KUNDNUMMER                              
002400     03 MOD-IDKUNDRF-KY      PIC X(10).                                   
002500*                                 KUNDENS REFERENS (ORDERID)              
002600     03 MOD-IDPURAD-KY       PIC 9(4).                                    
002700*                                 RADNUMMER PÅ PACKUNDERLAG               
002800     03 MOD-IDORDER-KY       PIC 9(7).                                    
002900*                                 VOLVO PARTS ORDERNUMMER                 
003000     03 MOD-IDARTNR-KY       PIC 9(9).                                    
003100*                                 ARTIKELNUMMER                           
003200     03 MOD-IDLOPNR-KY       PIC 9(3).                                    
003300*                                 LÖPNUMMER                               
003400     03 MOD-IDPRODNR-KY      PIC 9(7).                                    
003500*                                 PRODUKTIONSNUMMER                       
003600     03 MOD-IDPLKLST-KY      PIC 9(3).                                    
003700*                                 PLOCKLISTNUMMER                         
003800     03 MOD-UPDATE           OCCURS 14 TIMES.                             
003900*                                 TABELL-UPDATE                           
004000        05 MOD-IDARTNR       PIC Z(8)9.                                   
004100*                                 ARTIKELNUMMER                           
004200     03 MOD-UPDATE           OCCURS 14 TIMES.                             
004300*                                 TABELL-UPDATE                           
004400        05 MOD-KVBEART       PIC Z(5)9.                                   
004500*                                 BESTÄLLT ANTAL STYCKEN                  
004600     03 MOD-UPDATE           OCCURS 14 TIMES.                             
004700*                                 TABELL-UPDATE                           
004800        05 MOD-KVLEVART-FA   PIC Z(5)9.                                   
004900*                                 LEVERERAT ANTAL STYCK                   
005000     03 MOD-UPDATE           OCCURS 14 TIMES.                             
005100*                                 TABELL-UPDATE                           
005200        05 MOD-KVLEVART-PA   PIC Z(5)9.                                   
005300*                                 LEVERERAT ANTAL STYCK                   
005400     03 MOD-UPDATE           OCCURS 14 TIMES.                             
005500*                                 TABELL-UPDATE                           
005600        05 MOD-KVRO          PIC Z(5)9.                                   
005700*                                 ANTAL RESTNOTERADE ARTIKLAR             
005800     03 MOD-UPDATE           OCCURS 14 TIMES.                             
005900*                                 TABELL-UPDATE                           
006000        05 MOD-TITPO         PIC Z(6).                                    
006100*                                 PLANERAD ORDERDATUM                     
006200     03 MOD-UPDATE           OCCURS 14 TIMES.                             
006300*                                 TABELL-UPDATE                           
006400        05 MOD-KVANNANT      PIC Z(5)9.                                   
006500*                                 ANNULLERAT ANTAL ARTIKLAR               
006600     03 MOD-UPDATE           OCCURS 14 TIMES.                             
006700*                                 TABELL-UPDATE                           
006800        05 MOD-PRARTNTO      PIC Z(6)9.9(2).                              
006900*                                 ARTIKELPRIS NETTO                       
007000     03 MOD-UPDATE           OCCURS 14 TIMES.                             
007100*                                 TABELL-UPDATE                           
007200        05 MOD-IDORDNR-REF   PIC Z(6)9.                                   
007300*                                 ORDERNUMMER                             
007400     03 MOD-UPDATE           OCCURS 14 TIMES.                             
007500*                                 TABELL-UPDATE                           
007600        05 MOD-TETEXTX3      PIC X(3).                                    
007700     03 MOD-TEMFSINF         PIC X(55).                                   
007800*                                 INFORMATIONSMEDDELANDE                  
007900*** END OF VILMAII-COPY LENGTH= 1097 BYTES                                
