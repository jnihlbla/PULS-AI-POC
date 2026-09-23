000100 01  MOD-W4O57301.                                                        
000200*                                 COPYTEXT FÖR MOD W4O57301               
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
006200        05 MOD-LOPNRTAB      OCCURS 13 TIMES                              
006300                             INDEXED MOD-IX-1.                            
006400*                                  LOPNR TABELL                           
006500*                                                                         
006600           07 MOD-IDLOPNR    PIC Z9.                                      
006700*                                 LÖPNUMMER                               
006800     03 MOD-TEDDI            PIC X(3).                                    
006900     03 MOD-KDVALISO         PIC X(3).                                    
007000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007100     03 MOD-OPP-RAD.                                                      
007200*                                  RAD FÖR FÖRÄNDRINGAR                   
007300*                                                                         
007400        05 MOD-KVART-OPP-ATTR                                             
007500                             PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700        05 MOD-KVART-OPP     PIC X(7).                                    
007800*                                 ANTAL ARTNR PER BRYTBEGREPP             
007900        05 MOD-KDORDKL-OPP-ATTR                                           
008000                             PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200        05 MOD-KDORDKL-OPP   PIC X.                                       
008300*                                 ORDERKLASS                              
008400        05 MOD-KDFRAKT-OPP-ATTR                                           
008500                             PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700        05 MOD-KDFRAKT-OPP   PIC X(2).                                    
008800*                                 FRAKTSÄTT DC TILL KUND                  
008900        05 MOD-PRARTNTO-OPP-ATTR                                          
009000                             PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200        05 MOD-PRARTNTO-OPP  PIC X(10).                                   
009300*                                 ARTIKELPRIS NETTO                       
009400     03 MOD-RAD              OCCURS 13 TIMES                              
009500                             INDEXED MOD-IX-1.                            
009600*                                  RAD FÖR FÖRÄNDRINGAR                   
009700*                                                                         
009800        05 MOD-VALKOD        PIC X.                                       
009900*                                 ÄNDRINGSFLAGGA                          
010000        05 MOD-IDKUNDNR      PIC Z(6).                                    
010100*                                 KUNDNUMMER                              
010200        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400        05 MOD-IDARTNR       PIC Z(8)9.                                   
010500*                                 ARTIKELNUMMER                           
010600        05 MOD-KVART         PIC Z(6)9.                                   
010700*                                 ANTAL ARTNR PER BRYTBEGREPP             
010800        05 MOD-BEART         PIC X(15).                                   
010900*                                 ARTIKELBENÄMNING      BEART-002         
011000        05 MOD-KDORDKL       PIC Z.                                       
011100*                                 ORDERKLASS                              
011200        05 MOD-KDFRAKT       PIC Z9.                                      
011300*                                 FRAKTSÄTT DC TILL KUND                  
011400        05 MOD-IDORDNR       PIC Z(5).                                    
011500*                                 ORDERNUMMER                             
011600        05 MOD-KDFAKTYP      PIC X.                                       
011700*                                 FAKTURATYP                              
011800        05 MOD-KDFARLIG      PIC 9.                                       
011900*                                 KOD FÖR FARLIGT GODS                    
012000        05 MOD-PRARTNTO      PIC Z(7).Z(2).                               
012100*                                 ARTIKELPRIS NETTO                       
012200        05 MOD-TEASTRIX      PIC X.                                       
012300*                                 ASTERISK                                
012400        05 MOD-KDTPOTYP      PIC 9.                                       
012500*                                 TYP AV TIDPLANERAD ORDER                
012600        05 MOD-IDDC          PIC X(2).                                    
012700*                                 IDENTIFIERARE LAGER                     
012800     03 MOD-SPARADE-NYCKLAR-E.                                            
012900*                                   SPARADE NYCKLAR ENTER                 
013000        05 MOD-IDDISTR-SPAR-E                                             
013100                             PIC 9(4).                                    
013200*                                 DISTRIKTNUMMER                          
013300        05 MOD-IDKUNDNR-SPAR-E                                            
013400                             PIC 9(6).                                    
013500*                                 KUNDNUMMER                              
013600        05 MOD-IDARTNR-SPAR-E                                             
013700                             PIC 9(9).                                    
013800*                                 ARTIKELNUMMER                           
013900        05 MOD-KDORDKL-SPAR-E                                             
014000                             PIC 9.                                       
014100*                                 ORDERKLASS                              
014200        05 MOD-KDPRODSL-SPAR-E                                            
014300                             PIC 9(3).                                    
014400*                                 PRODUKTSLAG                             
014500        05 MOD-IDORDNR-SPAR-E                                             
014600                             PIC 9(5).                                    
014700*                                 ORDERNUMMER                             
014800        05 MOD-KDTPOTYP-SPAR-E                                            
014900                             PIC 9.                                       
015000*                                 TYP AV TIDPLANERAD ORDER                
015100        05 MOD-IDDC-SPAR-E   PIC X(2).                                    
015200*                                 IDENTIFIERARE LAGER                     
015300     03 MOD-TEMFSINF         PIC X(55).                                   
015400*                                 INFORMATIONSMEDDELANDE                  
015500*** END OF VILMAII-COPY LENGTH= 1120 BYTES                                
