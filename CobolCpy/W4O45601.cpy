000100 01  MOD-W4O45601.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDDISTR-IN       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MOD-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MOD-KDTRPDOCT-IN     PIC X.                                       
001600*                                 TYP AV TRANSPORTDOKUMENT                
001700     03 MOD-KDTRPDOCT-UT     PIC X.                                       
001800*                                 TYP AV TRANSPORTDOKUMENT                
001900     03 MOD-RAD              OCCURS 9 TIMES.                              
002000*                                 GRUPP MED RADER                         
002100        05 MOD-KDCMD-ATTR    PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-KDCMD         PIC X.                                       
002400*                                 RAD-UPPDATERINGSKOMMANDO                
002500*                                  BLANK  = INGENTING                     
002600*                                  D , B  = DELETE                        
002700*                                  R , Ä  = REPLACE                       
002800*                                  I , N  = INSERT                        
002900*                                  S , V  = SELECT                        
003000*                                  P , P  = PRINT                         
003100*                                  C , K  = COPY                          
003200        05 MOD-IDDC          PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400        05 MOD-IDDISTR       PIC Z(3)9.                                   
003500*                                 DISTRIKTNUMMER                          
003600        05 MOD-IDKUNDNR      PIC X(6).                                    
003700*                                 KUNDNUMMER                              
003800        05 MOD-KVCOPIES-KLIS PIC X.                                       
003900*                                 ANTAL COPIOR VID PRINTNING              
004000        05 MOD-KVCOPIES-VERS PIC X.                                       
004100*                                 ANTAL COPIOR VID PRINTNING              
004200        05 MOD-KVCOPIES-STAT PIC X.                                       
004300*                                 ANTAL COPIOR VID PRINTNING              
004400        05 MOD-KVCOPIES-SPED PIC X.                                       
004500*                                 ANTAL COPIOR VID PRINTNING              
004600        05 MOD-KVCOPIES-PACK PIC X.                                       
004700*                                 ANTAL COPIOR VID PRINTNING              
004800        05 MOD-KVCOPIES-GMTL PIC X.                                       
004900*                                 ANTAL COPIOR VID PRINTNING              
005000        05 MOD-KVCOPIES-KULB PIC X.                                       
005100*                                 ANTAL COPIOR VID PRINTNING              
005200        05 MOD-KVCOPIES-NAPR PIC X.                                       
005300*                                 ANTAL COPIOR VID PRINTNING              
005400        05 MOD-KVCOPIES-BLAD PIC X.                                       
005500*                                 ANTAL COPIOR VID PRINTNING              
005600        05 MOD-KVCOPIES-TRPT PIC X.                                       
005700*                                 ANTAL COPIOR VID PRINTNING              
005800        05 MOD-IDDC-REC      PIC X(2).                                    
005900*                                 MOTTAGANDE LAGER                        
006000        05 MOD-IDLTERM       PIC X(8).                                    
006100*                                 LOGISKT TERMINALNAMN                    
006200        05 MOD-KVDAGAR       PIC Z(2)9.                                   
006300*                                 ANTAL DAGAR                             
006400        05 MOD-IDUSER        PIC X(8).                                    
006500*                                 ANVÄNDARENS SÄKERHETS ID                
006600        05 MOD-TIUPPDAT      PIC 9(6).                                    
006700*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
006800        05 MOD-FLSKRIV-ONDEM PIC X.                                       
006900*                                 J/Y = SKRIV BEGÄRD LISTA                
007000     03 MOD-UPD.                                                          
007100*                                 UPPDATERINGSRAD                         
007200        05 MOD-IDDC-UPD-ATTR PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-IDDC-UPD      PIC X(2).                                    
007500*                                 IDENTIFIERARE LAGER                     
007600        05 MOD-IDDISTR-UPD-ATTR                                           
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900        05 MOD-IDDISTR-UPD   PIC Z(3)9.                                   
008000*                                 DISTRIKTNUMMER                          
008100        05 MOD-IDKUNDNR-UPD-ATTR                                          
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-IDKUNDNR-UPD  PIC X(6).                                    
008500*                                 KUNDNUMMER                              
008600        05 MOD-KVCOPIES-KLIS-UPD-ATTR                                     
008700                             PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900        05 MOD-KVCOPIES-KLIS-UPD                                          
009000                             PIC X.                                       
009100*                                 ANTAL COPIOR VID PRINTNING              
009200        05 MOD-KVCOPIES-VERS-UPD-ATTR                                     
009300                             PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500        05 MOD-KVCOPIES-VERS-UPD                                          
009600                             PIC X.                                       
009700*                                 ANTAL COPIOR VID PRINTNING              
009800        05 MOD-KVCOPIES-STAT-UPD-ATTR                                     
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100        05 MOD-KVCOPIES-STAT-UPD                                          
010200                             PIC X.                                       
010300*                                 ANTAL COPIOR VID PRINTNING              
010400        05 MOD-KVCOPIES-SPED-UPD-ATTR                                     
010500                             PIC X(2).                                    
010600*                                 MFS ATTRIBUTFÄLT                        
010700        05 MOD-KVCOPIES-SPED-UPD                                          
010800                             PIC X.                                       
010900*                                 ANTAL COPIOR VID PRINTNING              
011000        05 MOD-KVCOPIES-PACK-UPD-ATTR                                     
011100                             PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300        05 MOD-KVCOPIES-PACK-UPD                                          
011400                             PIC X.                                       
011500*                                 ANTAL COPIOR VID PRINTNING              
011600        05 MOD-KVCOPIES-GMTL-UPD-ATTR                                     
011700                             PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900        05 MOD-KVCOPIES-GMTL-UPD                                          
012000                             PIC X.                                       
012100*                                 ANTAL COPIOR VID PRINTNING              
012200        05 MOD-KVCOPIES-KULB-UPD-ATTR                                     
012300                             PIC X(2).                                    
012400*                                 MFS ATTRIBUTFÄLT                        
012500        05 MOD-KVCOPIES-KULB-UPD                                          
012600                             PIC X.                                       
012700*                                 ANTAL COPIOR VID PRINTNING              
012800        05 MOD-KVCOPIES-NAPR-UPD-ATTR                                     
012900                             PIC X(2).                                    
013000*                                 MFS ATTRIBUTFÄLT                        
013100        05 MOD-KVCOPIES-NAPR-UPD                                          
013200                             PIC X.                                       
013300*                                 ANTAL COPIOR VID PRINTNING              
013400        05 MOD-KVCOPIES-BLAD-UPD-ATTR                                     
013500                             PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700        05 MOD-KVCOPIES-BLAD-UPD                                          
013800                             PIC X.                                       
013900*                                 ANTAL COPIOR VID PRINTNING              
014000        05 MOD-KVCOPIES-TRPT-UPD-ATTR                                     
014100                             PIC X(2).                                    
014200*                                 MFS ATTRIBUTFÄLT                        
014300        05 MOD-KVCOPIES-TRPT-UPD                                          
014400                             PIC X.                                       
014500*                                 ANTAL COPIOR VID PRINTNING              
014600        05 MOD-IDDC-REC-UPD-ATTR                                          
014700                             PIC X(2).                                    
014800*                                 MFS ATTRIBUTFÄLT                        
014900        05 MOD-IDDC-REC-UPD  PIC X(2).                                    
015000*                                 MOTTAGANDE LAGER                        
015100        05 MOD-IDLTERM-UPD-ATTR                                           
015200                             PIC X(2).                                    
015300*                                 MFS ATTRIBUTFÄLT                        
015400        05 MOD-IDLTERM-UPD   PIC X(8).                                    
015500*                                 LOGISKT TERMINALNAMN                    
015600        05 MOD-KVDAGAR-UPD-ATTR                                           
015700                             PIC X(2).                                    
015800*                                 MFS ATTRIBUTFÄLT                        
015900        05 MOD-KVDAGAR-UPD   PIC Z(2)9.                                   
016000*                                 ANTAL DAGAR                             
016100        05 MOD-FLSKRIV-ONDEM-UPD-ATTR                                     
016200                             PIC X(2).                                    
016300*                                 MFS ATTRIBUTFÄLT                        
016400        05 MOD-FLSKRIV-ONDEM-UPD                                          
016500                             PIC X.                                       
016600*                                 J/Y = SKRIV BEGÄRD LISTA                
016700     03 MOD-TEMFSINF         PIC X(55).                                   
016800*                                 INFORMATIONSMEDDELANDE                  
016900*** END OF VILMAII-COPY LENGTH= 660 BYTES                                 
