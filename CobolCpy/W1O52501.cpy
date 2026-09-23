000100 01  MOD-W1O52501.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 1525.             
000300*                                 BESTÄLLNING AV KATALOG-                 
000400*                                 OMBRYTNING                              
000500     03 MOD-IDTRANS          PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900     03 MOD-IDCATNR-IN-ATTR  PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100     03 MOD-IDCATNR-IN       PIC X(5).                                    
001200*                                 KATALOG-ID                              
001300     03 MOD-IDCATNR-UT       PIC X(5).                                    
001400*                                 KATALOG-ID                              
001500     03 MOD-BEMASTER         PIC X(12).                                   
001600*                                 MASTERNAMN FÖR FORDON                   
001700     03 MOD-KAT-INFO.                                                     
001800        05 MOD-KAT-IDARTNR-ATTR                                           
001900                             PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100        05 MOD-KAT-IDARTNR   PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300        05 MOD-IDSKYLT-1-ATTR                                             
002400                             PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600        05 MOD-IDSKYLT-1     PIC X(3).                                    
002700*                                 NATIONALITETSTECKEN                     
002800*                                 SPRÅKIDENTIFIKATION                     
002900        05 MOD-IDSKYLT-2-ATTR                                             
003000                             PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-IDSKYLT-2     PIC X(3).                                    
003300*                                 NATIONALITETSTECKEN                     
003400*                                 SPRÅKIDENTIFIKATION                     
003500        05 MOD-IDSKYLT-3-ATTR                                             
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-IDSKYLT-3     PIC X(3).                                    
003900*                                 NATIONALITETSTECKEN                     
004000*                                 SPRÅKIDENTIFIKATION                     
004100        05 MOD-IDSKYLT-4-ATTR                                             
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-IDSKYLT-4     PIC X(3).                                    
004500*                                 NATIONALITETSTECKEN                     
004600*                                 SPRÅKIDENTIFIKATION                     
004700        05 MOD-IDSKYLT-5-ATTR                                             
004800                             PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000        05 MOD-IDSKYLT-5     PIC X(3).                                    
005100*                                 NATIONALITETSTECKEN                     
005200*                                 SPRÅKIDENTIFIKATION                     
005300        05 MOD-IDSKYLT-6-ATTR                                             
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-IDSKYLT-6     PIC X(3).                                    
005700*                                 NATIONALITETSTECKEN                     
005800*                                 SPRÅKIDENTIFIKATION                     
005900     03 MOD-KDCATPUB-R-ATTR  PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-KDCATPUB-R       PIC X(3).                                    
006200*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
006300     03 MOD-TIOMBRYT-ORD     PIC X(6).                                    
006400*                                 DATUM DÅ OMBRYTNING BEORDRATS           
006500     03 MOD-KDBEH-KATALOGTYP-ATTR                                         
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-KDBEH-KATALOGTYP PIC X(2).                                    
006900*                                 MFS BEHANDLING AV INPUTFÄLT             
007000     03 MOD-TIAAVV-PUBL-ATTR PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-TIAAVV-PUBL      PIC X(4).                                    
007300*                                 ÅR - VECKA  (ÅÅVV)                      
007400     03 MOD-FLJANEJ-SPADAT-ATTR                                           
007500                             PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 MOD-FLJANEJ-SPADAT   PIC X(2).                                    
007800*                                 MFS BEHANDLING AV INPUTFÄLT             
007900     03 MOD-FLJANEJ-MASTER-ATTR                                           
008000                             PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 MOD-FLJANEJ-MASTER   PIC X(2).                                    
008300*                                 MFS BEHANDLING AV INPUTFÄLT             
008400     03 MOD-FLJANEJ-SPRAKKAT-ATTR                                         
008500                             PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700     03 MOD-FLJANEJ-SPRAKKAT PIC X(2).                                    
008800*                                 MFS BEHANDLING AV INPUTFÄLT             
008900     03 MOD-BEKOM-1-ATTR     PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100     03 MOD-BEKOM-1          PIC X(50).                                   
009200*                                 KOMMENTAR/INSTRUKTION                   
009300     03 MOD-BEKOM-2-ATTR     PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-BEKOM-2          PIC X(50).                                   
009600*                                 KOMMENTAR/INSTRUKTION                   
009700     03 MOD-TEMFSINF         PIC X(55).                                   
009800*                                 INFORMATIONSMEDDELANDE                  
009900*** END OF VILMAII-COPY LENGTH= 301 BYTES                                 
