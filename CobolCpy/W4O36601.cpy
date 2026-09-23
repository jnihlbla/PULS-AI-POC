000100 01  MOD-W4O36601.                                                        
000200*                                 MOD-COPYTEXT FÖR W4036600               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDPRCTAB-IN      PIC X(2).                                    
000800*                                 PRCTABELLIDENTITET                      
000900     03 MOD-IDPRCTAB-UT      PIC X(2).                                    
001000*                                 PRCTABELLIDENTITET                      
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDRADNR-ENTER    PIC 9(4).                                    
001600*                                 RADNUMMER                               
001700     03 MOD-IDRADNR-NEXT     PIC 9(4).                                    
001800*                                 RADNUMMER                               
001900     03 MOD-TABELLRAD        OCCURS 13 TIMES.                             
002000*                                 GRUPP MED TABELLRADER                   
002100        05 MOD-IDRADNR-ATTR  PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-IDRADNR       PIC Z(3)9.                                   
002400*                                 RADNUMMER                               
002500        05 MOD-IDGMTOMR-FOM-ATTR                                          
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 MOD-IDGMTOMR-FOM  PIC Z(3)9.                                   
002900*                                 GODSMOTTAGAREOMRÅDE FRÅN                
003000        05 MOD-IDGMTOMR-STRECK                                            
003100                             PIC X.                                       
003200        05 MOD-IDGMTOMR-TOM-ATTR                                          
003300                             PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-IDGMTOMR-TOM  PIC Z(3)9.                                   
003600*                                 GODSMOTTAGAREOMRÅDE TILL                
003700        05 MOD-KDPRODKL-FOM-ATTR                                          
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-KDPRODKL-FOM  PIC X.                                       
004100*                                 PRODUKTIONSKLASS FRÅN                   
004200        05 MOD-KDPRODKL-STRECK                                            
004300                             PIC X.                                       
004400        05 MOD-KDPRODKL-TOM-ATTR                                          
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-KDPRODKL-TOM  PIC X.                                       
004800*                                 PRODUKTIONSKLASS TILL                   
004900        05 MOD-KDFRAKT-FOM-ATTR                                           
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-KDFRAKT-FOM   PIC Z9.                                      
005300*                                 FRAKTSÄTT FRÅN OCH MED                  
005400        05 MOD-KDFRAKT-STRECK                                             
005500                             PIC X.                                       
005600        05 MOD-KDFRAKT-TOM-ATTR                                           
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-KDFRAKT-TOM   PIC Z9.                                      
006000*                                 FRAKTSÄTT TILL OCH MED                  
006100        05 MOD-IDTRP-ATTR    PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-IDTRP.                                                     
006400*                                 TRANSPORTIDENTITET                      
006500           07 MOD-IDTRPLOS   PIC X(3).                                    
006600*                                 TRANSPORTLÖSNING                        
006700           07 MOD-IDTRPVAR   PIC X(2).                                    
006800*                                 TRANSPORTLÖSNINGSGRUPP                  
006900        05 MOD-IDHLO-FOM-ATTR                                             
007000                             PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200        05 MOD-IDHLO-FOM     PIC Z9.                                      
007300*                                 HUVUDLAGEROMRÅDE FRÅN OCH MED           
007400        05 MOD-IDHLO-STRECK  PIC X.                                       
007500        05 MOD-IDHLO-TOM-ATTR                                             
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-IDHLO-TOM     PIC Z9.                                      
007900*                                 HUVUDLAGEROMRÅDE TILL OCH MED           
008000        05 MOD-IDPTIDTAB-ATTR                                             
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-IDPTIDTAB     PIC Z9.                                      
008400*                                 PRODUKTIONSTIDTABELLSIDENTITET          
008500        05 MOD-IDPRC-ATTR    PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700        05 MOD-IDPRC.                                                     
008800*                                 PRODUKTIONSKANAL                        
008900           07 MOD-IDPRCBAS   PIC X(3).                                    
009000*                                 PRC-BAS                                 
009100           07 MOD-IDPRCVAR   PIC X.                                       
009200*                                 PRC-VARIANT                             
009300     03 MOD-IDRADNR-IN-ATTR  PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-IDRADNR-IN       PIC X(2).                                    
009600*                                 MFS BEHANDLING AV INPUTFÄLT             
009700     03 MOD-INDATA.                                                       
009800*                                 INDATAFÄLT                              
009900        05 MOD-IDGMTOMR-F-UPP-ATTR                                        
010000                             PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200        05 MOD-IDGMTOMR-F-UPP                                             
010300                             PIC X(2).                                    
010400*                                 MFS BEHANDLING AV INPUTFÄLT             
010500        05 MOD-IDGMTOMR-T-UPP-ATTR                                        
010600                             PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800        05 MOD-IDGMTOMR-T-UPP                                             
010900                             PIC X(2).                                    
011000*                                 MFS BEHANDLING AV INPUTFÄLT             
011100        05 MOD-KDPRODKL-F-UPP-ATTR                                        
011200                             PIC X(2).                                    
011300*                                 MFS ATTRIBUTFÄLT                        
011400        05 MOD-KDPRODKL-F-UPP                                             
011500                             PIC X(2).                                    
011600*                                 MFS BEHANDLING AV INPUTFÄLT             
011700        05 MOD-KDPRODKL-T-UPP-ATTR                                        
011800                             PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000        05 MOD-KDPRODKL-T-UPP                                             
012100                             PIC X(2).                                    
012200*                                 MFS BEHANDLING AV INPUTFÄLT             
012300        05 MOD-KDFRAKT-F-UPP-ATTR                                         
012400                             PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600        05 MOD-KDFRAKT-F-UPP PIC X(2).                                    
012700*                                 MFS BEHANDLING AV INPUTFÄLT             
012800        05 MOD-KDFRAKT-T-UPP-ATTR                                         
012900                             PIC X(2).                                    
013000*                                 MFS ATTRIBUTFÄLT                        
013100        05 MOD-KDFRAKT-T-UPP PIC X(2).                                    
013200*                                 MFS BEHANDLING AV INPUTFÄLT             
013300        05 MOD-IDTRP-UPP-ATTR                                             
013400                             PIC X(2).                                    
013500*                                 MFS ATTRIBUTFÄLT                        
013600        05 MOD-IDTRP-UPP     PIC X(2).                                    
013700*                                 MFS BEHANDLING AV INPUTFÄLT             
013800        05 MOD-IDHLO-F-UPP-ATTR                                           
013900                             PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100        05 MOD-IDHLO-F-UPP   PIC X(2).                                    
014200*                                 MFS BEHANDLING AV INPUTFÄLT             
014300        05 MOD-IDHLO-T-UPP-ATTR                                           
014400                             PIC X(2).                                    
014500*                                 MFS ATTRIBUTFÄLT                        
014600        05 MOD-IDHLO-T-UPP   PIC X(2).                                    
014700*                                 MFS BEHANDLING AV INPUTFÄLT             
014800        05 MOD-IDPTIDTAB-UPP-ATTR                                         
014900                             PIC X(2).                                    
015000*                                 MFS ATTRIBUTFÄLT                        
015100        05 MOD-IDPTIDTAB-UPP PIC X(2).                                    
015200*                                 MFS BEHANDLING AV INPUTFÄLT             
015300        05 MOD-IDPRC-UPP-ATTR                                             
015400                             PIC X(2).                                    
015500*                                 MFS ATTRIBUTFÄLT                        
015600        05 MOD-IDPRC-UPP     PIC X(2).                                    
015700*                                 MFS BEHANDLING AV INPUTFÄLT             
015800     03 MOD-TEMFSINF         PIC X(55).                                   
015900*                                 INFORMATIONSMEDDELANDE                  
016000*** END COPY W4O36601    LENGTH=956                                       
