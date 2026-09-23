000100 01  MOD-W1O14201.                                                        
000200*                                 MOD-COPYTEXT FÖR W1014200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDBERED-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDBERED-UT       PIC X(2).                                    
001000*                                 BEREDARENUMMER                          
001100     03 MOD-IDAO-IN          PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDAO-UT          PIC X(10).                                   
001400*                                 ÄNDRINGSORDERNUMMER                     
001500     03 MOD-IDPROJ-IN        PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDPROJ-UT        PIC X(4).                                    
001800*                                 PARTS PROJEKTIDENTITET                  
001900     03 MOD-IDAO-LO          PIC X(10).                                   
002000*                                 ÄNDRINGSORDERNUMMER                     
002100     03 MOD-IDAO-HI          PIC X(10).                                   
002200*                                 ÄNDRINGSORDERNUMMER                     
002300     03 MOD-IDARTNR-LO       PIC X(9).                                    
002400*                                 ARTIKELNUMMER                           
002500     03 MOD-IDARTNR-HI       PIC X(9).                                    
002600*                                 ARTIKELNUMMER                           
002700     03 MOD-INFO-RAD         OCCURS 13 TIMES.                             
002800*                                 RADINFORMATION                          
002900        05 MOD-SELECT-ARTIKEL                                             
003000                             PIC X.                                       
003100        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-IDARTNR       PIC Z(8)9.                                   
003400*                                 ARTIKELNUMMER                           
003500        05 MOD-BEART-SVE     PIC X(25).                                   
003600*                                 SVENSK ARTIKELBENÄMNING                 
003700        05 MOD-IDAO          PIC X(10).                                   
003800*                                 ÄNDRINGSORDERNUMMER                     
003900        05 FILLER            PIC X.                                       
004000        05 MOD-IDFKNGRP      PIC Z(3)9.                                   
004100*                                 FUNKTIONSGRUPP                          
004200        05 MOD-RS-IN-ATTR    PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-RS-IN         PIC X(2).                                    
004500*                                 MFS BEHANDLING AV INPUTFÄLT             
004600        05 MOD-TISERLEV      PIC 9(6).                                    
004700*                                 SERIELEVERANS START                     
004800        05 MOD-IDPROJ        PIC X(4).                                    
004900*                                 PARTS PROJEKTIDENTITET                  
005000        05 MOD-TISLUBER      PIC 9(6).                                    
005100*                                 BEREDNINGS SLUT                         
005200        05 MOD-NEDB-IN-ATTR  PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-NEDB-IN       PIC 9(6).                                    
005500*                                 SLUTTID NEDBRYTNING                     
005600        05 MOD-NY-IDBERED-IN-ATTR                                         
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-NY-IDBERED-IN PIC X(2).                                    
006000*                                 MFS BEHANDLING AV INPUTFÄLT             
006100        05 MOD-TETEKNIK      PIC X.                                       
006200        05 MOD-BORTTAG-IN-ATTR                                            
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-BORTTAG-IN    PIC X(2).                                    
006600*                                 MFS BEHANDLING AV INPUTFÄLT             
006700        05 MOD-KONVERTERAD-ARTIKEL                                        
006800                             PIC X.                                       
006900     03 MOD-TEMFSINF         PIC X(61).                                   
007000*                                 INFORMATIONSMEDDELANDE                  
007100*** END COPY W1O14201C0  LENGTH=1335                                      
