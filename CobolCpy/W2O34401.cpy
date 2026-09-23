000100 01  MOD-W2O34401.                                                        
000200*                                 MOD-COPYTEXT FÖR W2034400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-BEART            PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700     03 MOD-TABELLRAD        OCCURS 11 TIMES.                             
001800        05 MOD-CMD-ATTR      PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000        05 MOD-CMD           PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200        05 MOD-IDDC          PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400        05 MOD-ADLAGOMR-CD-ATTR                                           
002500                             PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-ADLAGOMR-CD-IN                                             
002800                             PIC Z9.                                      
002900*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
003000        05 MOD-ADLAGOMR-CD   PIC Z9.                                      
003100*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
003200        05 MOD-KVDAGAR-CDBEH-ATTR                                         
003300                             PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-KVDAGAR-CDBEH-IN                                           
003600                             PIC Z9.                                      
003700*                                 NO. OF DAYS TO BE USED WHEN             
003800*                                 CALCULATING CD REFILLORDERS             
003900        05 MOD-KVDAGAR-CDBEH PIC Z9.                                      
004000*                                 NO. OF DAYS TO BE USED WHEN             
004100*                                 CALCULATING CD REFILLORDERS             
004200        05 MOD-FLCDREL-ATTR  PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-FLCDREL-IN    PIC X(2).                                    
004500*                                 MFS BEHANDLING AV INPUTFÄLT             
004600        05 MOD-FLCDREL       PIC X.                                       
004700*                                 OMGÅENDE RELEASE AV CD-REFILL           
004800        05 MOD-TIDATUM-CROSS-ATTR                                         
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-TIDATUM-CROSS-IN                                           
005200                             PIC 9(6).                                    
005300*                                 STARTDAG CROSS DOCKING FÖRDRÖJN         
005400        05 MOD-TIDATUM-CROSS PIC 9(6).                                    
005500*                                 STARTDAG CROSS DOCKING FÖRDRÖJN         
005600     03 MOD-NY-IDDC-ATTR     PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-NY-IDDC          PIC X(2).                                    
005900*                                 IDENTIFIERARE LAGER                     
006000     03 MOD-NY-ADLAGOMR-CD-ATTR                                           
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-NY-ADLAGOMR-CD   PIC Z9.                                      
006400*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
006500     03 MOD-NY-KVDAGAR-CDBEH-ATTR                                         
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-NY-KVDAGAR-CDBEH PIC Z9.                                      
006900*                                 NO. OF DAYS TO BE USED WHEN             
007000*                                 CALCULATING CD REFILLORDERS             
007100     03 MOD-NY-FLCDREL-ATTR  PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 MOD-NY-FLCDREL       PIC X(2).                                    
007400*                                 MFS BEHANDLING AV INPUTFÄLT             
007500     03 MOD-TEMFSINF         PIC X(55).                                   
007600*                                 INFORMATIONSMEDDELANDE                  
007700*** END OF VILMAII-COPY LENGTH= 569 BYTES                                 
