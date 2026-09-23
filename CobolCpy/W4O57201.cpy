000100 01  MOD-W4O57201.                                                        
000200*                                 COPYTEXT FÖR MOD W4O57201               
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-IDARTNR-IN       PIC X(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 MOD-IDARTNR-UT       PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 MOD-KDORDKL-IN       PIC X.                                       
002100*                                 ORDERKLASS                              
002200     03 MOD-KDORDKL-UT       PIC X.                                       
002300*                                 ORDERKLASS                              
002400     03 MOD-KDPRODSL-IN      PIC X(2).                                    
002500*                                 PRODUKTSLAG                             
002600     03 MOD-KDPRODSL-UT      PIC X(2).                                    
002700*                                 PRODUKTSLAG                             
002800     03 MOD-IDORDNR-IN       PIC X(5).                                    
002900*                                 ORDERNUMMER                             
003000     03 MOD-IDORDNR-UT       PIC X(5).                                    
003100*                                 ORDERNUMMER                             
003200     03 MOD-KDTPOTYP-IN      PIC X.                                       
003300*                                 TYP AV TIDPLANERAD ORDER                
003400     03 MOD-KDTPOTYP-UT      PIC X.                                       
003500*                                 TYP AV TIDPLANERAD ORDER                
003600     03 MOD-IDDC-IN          PIC X(2).                                    
003700*                                 IDENTIFIERARE LAGER                     
003800     03 MOD-IDDC-UT          PIC X(2).                                    
003900*                                 IDENTIFIERARE LAGER                     
004000     03 MOD-SPARADE-NYCKLAR.                                              
004100*                                       SPARADE NYCKLAR PF8               
004200        05 MOD-IDDISTR-SPAR  PIC 9(4).                                    
004300*                                 DISTRIKTNUMMER                          
004400        05 MOD-IDKUNDNR-SPAR PIC 9(6).                                    
004500*                                 KUNDNUMMER                              
004600        05 MOD-IDARTNR-SPAR  PIC 9(9).                                    
004700*                                 ARTIKELNUMMER                           
004800        05 MOD-KDORDKL-SPAR  PIC 9.                                       
004900*                                 ORDERKLASS                              
005000        05 MOD-KDPRODSL-SPAR PIC 9(2).                                    
005100*                                 PRODUKTSLAG                             
005200        05 MOD-IDORDNR-SPAR  PIC 9(5).                                    
005300*                                 ORDERNUMMER                             
005400        05 MOD-KDTPOTYP-SPAR PIC 9.                                       
005500*                                 TYP AV TIDPLANERAD ORDER                
005600        05 MOD-KVRAD-SPAR    PIC 9(6).                                    
005700*                                 ANTAL ORDERRADER                        
005800        05 MOD-IDLOPNR-SPAR  PIC 9(2).                                    
005900*                                 LÖPNUMMER                               
006000        05 MOD-IDDC-SPAR     PIC X(2).                                    
006100*                                 IDENTIFIERARE LAGER                     
006200        05 MOD-LOPNRTAB      OCCURS 13 TIMES.                             
006300*                                  LOPNR TABELL                           
006400*                                                                         
006500           07 MOD-IDLOPNR    PIC Z9.                                      
006600*                                 LÖPNUMMER                               
006700     03 MOD-TEDDI            PIC X(3).                                    
006800     03 MOD-KDVALISO         PIC X(3).                                    
006900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007000     03 MOD-OPP-RAD.                                                      
007100*                                  RAD FÖR FÖRÄNDRINGAR                   
007200*                                                                         
007300        05 MOD-KVART-OPP-ATTR                                             
007400                             PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600        05 MOD-KVART-OPP     PIC X(7).                                    
007700*                                 ANTAL ARTNR PER BRYTBEGREPP             
007800        05 MOD-KDORDKL-OPP-ATTR                                           
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100        05 MOD-KDORDKL-OPP   PIC X.                                       
008200*                                 ORDERKLASS                              
008300        05 MOD-KDFRAKT-OPP-ATTR                                           
008400                             PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-KDFRAKT-OPP   PIC X(2).                                    
008700*                                 FRAKTSÄTT DC TILL KUND                  
008800        05 MOD-PRARTNTO-OPP-ATTR                                          
008900                             PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100        05 MOD-PRARTNTO-OPP  PIC X(10).                                   
009200*                                 ARTIKELPRIS NETTO                       
009300     03 MOD-RAD              OCCURS 13 TIMES.                             
009400*                                  RAD FÖR FÖRÄNDRINGAR                   
009500*                                                                         
009600        05 MOD-VALKOD        PIC X.                                       
009700*                                 ÄNDRINGSFLAGGA                          
009800        05 MOD-IDKUNDNR      PIC Z(6).                                    
009900*                                 KUNDNUMMER                              
010000        05 MOD-IDARTNR       PIC Z(8)9.                                   
010100*                                 ARTIKELNUMMER                           
010200        05 MOD-KVART         PIC Z(6)9.                                   
010300*                                 ANTAL ARTNR PER BRYTBEGREPP             
010400        05 MOD-IDORDNR       PIC Z(5).                                    
010500*                                 ORDERNUMMER                             
010600        05 MOD-KDORDKL       PIC Z.                                       
010700*                                 ORDERKLASS                              
010800        05 MOD-KDFRAKT       PIC Z(3).                                    
010900*                                 FRAKTSÄTT DC TILL KUND                  
011000        05 MOD-TITPO         PIC 9(6).                                    
011100*                                 PLANERAD ORDERDATUM                     
011200        05 MOD-PRARTNTO      PIC Z(7).Z(2).                               
011300*                                 ARTIKELPRIS NETTO                       
011400        05 MOD-TEASTRIX      PIC X.                                       
011500*                                 ASTERISK                                
011600        05 MOD-KVQPACK-1     PIC Z(4)9.                                   
011700*                                 ANTAL I Q1 FÖRPACKNING                  
011800        05 MOD-KDRAPRIO      PIC Z(2)9.                                   
011900*                                 PRIORITETSKOD PÅ RADEN                  
012000        05 MOD-IDDC          PIC X(2).                                    
012100*                                 IDENTIFIERARE LAGER                     
012200     03 MOD-SPARADE-NYCKLAR-E.                                            
012300*                                   SPARADE NYCKLAR ENTER                 
012400        05 MOD-IDDISTR-SPAR-E                                             
012500                             PIC 9(4).                                    
012600*                                 DISTRIKTNUMMER                          
012700        05 MOD-IDKUNDNR-SPAR-E                                            
012800                             PIC 9(6).                                    
012900*                                 KUNDNUMMER                              
013000        05 MOD-IDARTNR-SPAR-E                                             
013100                             PIC 9(9).                                    
013200*                                 ARTIKELNUMMER                           
013300        05 MOD-KDORDKL-SPAR-E                                             
013400                             PIC 9.                                       
013500*                                 ORDERKLASS                              
013600        05 MOD-KDPRODSL-SPAR-E                                            
013700                             PIC 9(3).                                    
013800*                                 PRODUKTSLAG                             
013900        05 MOD-IDORDNR-SPAR-E                                             
014000                             PIC 9(5).                                    
014100*                                 ORDERNUMMER                             
014200        05 MOD-KDTPOTYP-SPAR-E                                            
014300                             PIC 9.                                       
014400*                                 TYP AV TIDPLANERAD ORDER                
014500        05 MOD-IDDC-SPAR-E   PIC X(2).                                    
014600*                                 IDENTIFIERARE LAGER                     
014700     03 MOD-TEMFSINF         PIC X(55).                                   
014800*                                 INFORMATIONSMEDDELANDE                  
014900*** END OF VILMAII-COPY LENGTH= 1055 BYTES                                
