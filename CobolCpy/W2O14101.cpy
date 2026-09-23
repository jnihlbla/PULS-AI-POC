000100 01  MOD-W2O14101-CTX.                                                    
000200*                                 MOD-COPYTEXT FÖR W2014100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDANSK-FROM-IN   PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDANSK-FROM-UT   PIC X(3).                                    
001000*                                 ANSKAFFARNUMMER                         
001100     03 MOD-IDANSK-TOM-IN    PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDANSK-TOM-UT    PIC X(3).                                    
001400*                                 ANSKAFFARNUMMER                         
001500     03 MOD-IDPROJ-IN        PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDPROJ-UT        PIC X(4).                                    
001800*                                 PARTS PROJEKTIDENTITET                  
001900     03 MOD-ANT-ARTIKLAR     PIC X(5).                                    
002000*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
002100     03 MOD-ANT-PASS-INLV    PIC X(5).                                    
002200*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
002300     03 MOD-ANT-PISK         PIC X(5).                                    
002400*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
002500     03 MOD-HELA-BASEN-LAEST PIC X.                                       
002600     03 MOD-IDANSK-LO        PIC X(3).                                    
002700*                                 ANSKAFFARNUMMER                         
002800     03 MOD-IDANSK-HI        PIC X(3).                                    
002900*                                 ANSKAFFARNUMMER                         
003000     03 MOD-IDPROJ-LO        PIC X(4).                                    
003100*                                 PARTS PROJEKTIDENTITET                  
003200     03 MOD-IDPROJ-HI        PIC X(4).                                    
003300*                                 PARTS PROJEKTIDENTITET                  
003400     03 MOD-FLPISK-LO        PIC X.                                       
003500*                                 PISK ARTIKEL                            
003600     03 MOD-FLPISK-HI        PIC X.                                       
003700*                                 PISK ARTIKEL                            
003800     03 MOD-TIFINLEV-LO      PIC 9(6).                                    
003900*                                 PUBLICERINGSDATUM  (AAMMDD)             
004000     03 MOD-TIFINLEV-HI      PIC 9(6).                                    
004100*                                 PUBLICERINGSDATUM  (AAMMDD)             
004200     03 MOD-IDAO-LO          PIC X(10).                                   
004300*                                 ÄNDRINGSORDERNUMMER                     
004400     03 MOD-IDAO-HI          PIC X(10).                                   
004500*                                 ÄNDRINGSORDERNUMMER                     
004600     03 MOD-VISA-ANT-ARTIKLAR                                             
004700                             PIC Z(4)9.                                   
004800*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
004900     03 MOD-VISA-ANT-PASS-INLV                                            
005000                             PIC Z(4)9.                                   
005100*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
005200     03 MOD-VISA-ANT-PISK    PIC Z(4)9.                                   
005300*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
005400     03 MOD-W2O14101-002-GRP OCCURS 12 TIMES.                             
005500*                                 RADINFORMATION                          
005600        05 MOD-SELECT-ARTIKEL                                             
005700                             PIC X.                                       
005800        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000        05 MOD-IDARTNR       PIC Z(9).                                    
006100*                                 ARTIKELNUMMER                           
006200        05 MOD-IDPROJ        PIC X(4).                                    
006300*                                 PARTS PROJEKTIDENTITET                  
006400        05 MOD-FILLERX1      PIC X.                                       
006500        05 MOD-IDAO          PIC X(10).                                   
006600*                                 ÄNDRINGSORDERNUMMER                     
006700        05 MOD-FILLERX2      PIC X(2).                                    
006800        05 MOD-TIFINLEV      PIC 9(6).                                    
006900*                                 PUBLICERINGSDATUM  (AAMMDD)             
007000        05 MOD-FILLERX4      PIC X(4).                                    
007100        05 MOD-FLPISK        PIC X.                                       
007200*                                 PISK ARTIKEL                            
007300        05 MOD-FILLERX4      PIC X(4).                                    
007400        05 MOD-TIREGDAT      PIC 9(6).                                    
007500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007600        05 MOD-FILLERX3      PIC X(3).                                    
007700        05 MOD-IDFKNGRP      PIC Z(3)9.                                   
007800*                                 FUNKTIONSGRUPP                          
007900        05 MOD-FILLERX2      PIC X(2).                                    
008000        05 MOD-IDANSK        PIC Z(2)9.                                   
008100*                                 ANSKAFFARNUMMER                         
008200        05 MOD-NY-IDANSK-ATTR                                             
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500        05 MOD-NY-IDANSK     PIC Z(2)9.                                   
008600*                                 ANSKAFFARNUMMER                         
008700        05 MOD-FLCN          PIC X.                                       
008800     03 MOD-IDARTNR-LO       PIC X(9).                                    
008900*                                 ARTIKELNUMMER                           
009000     03 MOD-IDARTNR-HI       PIC X(9).                                    
009100*                                 ARTIKELNUMMER                           
009200     03 MOD-TEMFSINF         PIC X(55).                                   
009300*                                 INFORMATIONSMEDDELANDE                  
009400*** END OF VILMAII-COPY LENGTH= 1028 BYTES                                
