000100 01  MOD-W1O11301.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-IDARTNR-IN       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MOD-IDARTNR-UT       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDKORTNR-SPAR1   PIC X(3).                                    
001100*                                 KORTNUMMER                              
001200*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
001300     03 MOD-IDKORTNR-SPAR2   PIC X(3).                                    
001400*                                 KORTNUMMER                              
001500*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
001600     03 MOD-IDKORTNR-SPAR3   PIC X(3).                                    
001700*                                 KORTNUMMER                              
001800*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
001900     03 MOD-DIERS-ERS-ATTR   PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-DIERS-ERS        PIC Z(2)9.9(3).                              
002200*                                 KVANTITET I ERSÄTTN.                    
002300     03 MOD-KDERS-ATTR       PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-KDERS            PIC 9(2).                                    
002600*                                 ERSÄTTNINGSKOD                          
002700     03 MOD-IDAO-ATTR        PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-IDAO             PIC X(10).                                   
003000*                                 ÄNDRINGSORDERNUMMER                     
003100     03 MOD-TIERSDAT-PREL-ATTR                                            
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-TIERSDAT-PREL    PIC 9(5).                                    
003500*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
003600     03 MOD-UTRAD            OCCURS 9 TIMES.                              
003700        05 MOD-IDKORTNR-ATTR PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-IDKORTNR      PIC X(3).                                    
004000*                                 KORTNUMMER                              
004100*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
004200        05 MOD-FLTEXT        PIC X.                                       
004300*                                 FINNS TEXTINFORMATION ?                 
004400        05 MOD-IDARTNR-TILLK-ATTR                                         
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-IDARTNR-TILLK PIC Z(8)9.                                   
004800*                                 ARTIKELNUMMER                           
004900        05 MOD-DIERS-TILLK-ATTR                                           
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-DIERS-TILLK   PIC Z(2)9.9(3).                              
005300*                                 KVANTITET I ERSÄTTN.                    
005400        05 MOD-BEART         PIC X(25).                                   
005500*                                 ARTIKELBENÄMNING                        
005600        05 MOD-BEERS-ATTR    PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-BEERS         PIC X(20).                                   
005900*                                 ERSÄTTNINGSTEXT                         
006000     03 MOD-TEARTNOT-ATTR    PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-TEARTNOT         PIC X(40).                                   
006300*                                 ARTIKEL NOTERING                        
006400     03 MOD-IDUSER           PIC X(8).                                    
006500*                                 ANVÄNDARIDENTITET I RACF                
006600     03 MOD-FLKLAR-ATTR      PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-FLKLAR           PIC X.                                       
006900*                                 AVSLUTNINGSMARKERING                    
007000     03 MOD-KDERS-C1         PIC 9(2).                                    
007100*                                 ERSÄTTNINGSKOD                          
007200     03 MOD-KDERS-C2         PIC 9(2).                                    
007300*                                 ERSÄTTNINGSKOD                          
007400     03 MOD-TIERSDAT-REG     PIC 9(5).                                    
007500*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
007600     03 MOD-TIERSDAT-PREL-C1 PIC 9(5).                                    
007700*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
007800     03 MOD-TIERSDAT-PREL-C2 PIC 9(5).                                    
007900*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
008000     03 MOD-TIERSDAT         PIC 9(5).                                    
008100*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
008200     03 MOD-TEMFSINF         PIC X(61).                                   
008300*                                 INFORMATIONSMEDDELANDE                  
008400*** END COPY W1O11301C0  LENGTH=898                                       
