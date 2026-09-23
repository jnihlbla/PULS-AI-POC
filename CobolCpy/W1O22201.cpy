000100 01  MOD-W1O22201.                                                        
000200*                                 MOD-COPYTEXT FÖR W1022200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MOD-BELEVART-IN      PIC X(30).                                   
001000*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001100     03 MOD-IDSKYLT-IN-DOLT  PIC X(3).                                    
001200*                                 NATIONALITETSTECKEN                     
001300*                                 SPRÅKIDENTIFIKATION                     
001400     03 MOD-1002-SATS-IN-DOLT                                             
001500                             PIC X.                                       
001600*                                 ALLMÄN SVARSFLAGGA                      
001700     03 MOD-KDPRODSL-IN-DOLT PIC X(2).                                    
001800*                                 PRODUKTSLAG                             
001900     03 MOD-IDLEVNR-UT       PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100     03 MOD-IDARTNR-SPAR     PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-BELEVART-UT      PIC X(30).                                   
002400*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
002500     03 MOD-ARTIKELNR REDEFINES MOD-BELEVART-UT.                          
002600*                                                                         
002700        05 MOD-IDARTNR-UT    PIC X(9).                                    
002800*                                 ARTIKELNUMMER                           
002900        05 MOD-FILLER        PIC X(21).                                   
003000     03 MOD-IDSKYLT-UT-DOLT  PIC X(3).                                    
003100*                                 NATIONALITETSTECKEN                     
003200*                                 SPRÅKIDENTIFIKATION                     
003300     03 MOD-1002-SATS-UT-DOLT                                             
003400                             PIC X.                                       
003500*                                 ALLMÄN SVARSFLAGGA                      
003600     03 MOD-KDPRODSL-UT-DOLT PIC X(2).                                    
003700*                                 PRODUKTSLAG                             
003800     03 MOD-ANTAL-SEGMENT-ENTER                                           
003900                             PIC 9(3).                                    
004000     03 MOD-ANTAL-SEGMENT-NEXT                                            
004100                             PIC 9(3).                                    
004200     03 MOD-BEART-UTG-ART    PIC X(25).                                   
004300*                                 ARTIKELBENÄMNING                        
004400     03 MOD-AANGRA-ATTR      PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-AANGRA           PIC X.                                       
004700*                                 ALLMÄN SVARSFLAGGA                      
004800     03 MOD-IDAO-ATTR        PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-IDAO             PIC X(10).                                   
005100*                                 ÄNDRINGSORDERNUMMER                     
005200     03 MOD-BORTTAG-ATTR     PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-BORTTAG          PIC X.                                       
005500*                                 ALLMÄN SVARSFLAGGA                      
005600     03 MOD-TIAAVV-ATTR      PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-TIAAVV           PIC 9(4).                                    
005900*                                 ÅR - VECKA  (ÅÅVV)                      
006000     03 MOD-RADER            OCCURS 2 TIMES.                              
006100*                                 TILLKOMMANDE ARTIKLAR                   
006200        05 MOD-IDLEVNR-ATTR  PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-IDLEVNR       PIC X(5).                                    
006500*                                 LEVERANTÖRNUMMER                        
006600        05 MOD-BELEVART-ATTR PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-BELEVART      PIC X(30).                                   
006900*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
007000        05 MOD-REANTPSA-ATTR PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200        05 MOD-REANTPSA      PIC Z9.9(3).                                 
007300*                                 ANTAL PER SATS                          
007400        05 MOD-KDSORT-ATTR   PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600        05 MOD-KDSORT        PIC X(2).                                    
007700*                                 SORT-KOD                                
007800        05 MOD-BEART-ATTR    PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000        05 MOD-BEART         PIC X(25).                                   
008100*                                 ARTIKELBENÄMNING                        
008200        05 MOD-KDBENHOM-ATTR PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-KDBENHOM      PIC 9.                                       
008500*                                 HOMONYMKOD                              
008600        05 MOD-IDSTRTYP-ATTR PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800        05 MOD-IDSTRTYP      PIC X.                                       
008900*                                 STRUKTURTYP                             
009000        05 MOD-TESTRNOT-GRUPP                                             
009100                             OCCURS 2 TIMES.                              
009200*                                                                         
009300           07 MOD-TESTRNOT-ATTR                                           
009400                             PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600           07 MOD-TESTRNOT   PIC X(60).                                   
009700     03 MOD-KLAR-ATTR        PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900     03 MOD-KLAR             PIC X.                                       
010000*                                 ALLMÄN SVARSFLAGGA                      
010100     03 MOD-TEMFSINF         PIC X(55).                                   
010200*                                 INFORMATIONSMEDDELANDE                  
010300*** END OF VILMAII-COPY LENGTH= 664 BYTES                                 
