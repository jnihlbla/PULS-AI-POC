000100 01  MOD-W1O21201.                                                        
000200*                                 MOD-COPYTEXT FÖR W1021200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-STRNR-IN         PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-STRNR-UT         PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDSKYLT-IN       PIC X(3).                                    
001200*                                 NATIONALITETSTECKEN                     
001300*                                 SPRÅKIDENTIFIKATION                     
001400     03 MOD-IDSKYLT-UT       PIC X(3).                                    
001500*                                 NATIONALITETSTECKEN                     
001600*                                 SPRÅKIDENTIFIKATION                     
001700     03 MOD-IDRADNR-IN       PIC X(4).                                    
001800*                                 RADNUMMER                               
001900     03 MOD-IDRADNR-UT       PIC X(4).                                    
002000*                                 RADNUMMER                               
002100     03 MOD-IDRADNR-B        PIC X(4).                                    
002200*                                 RADNUMMER                               
002300     03 MOD-IDRADNR-K-ATTR   PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-IDRADNR-K        PIC X(4).                                    
002600*                                 RADNUMMER                               
002700     03 MOD-IDRADNR-F-ATTR   PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-IDRADNR-F        PIC X(4).                                    
003000*                                 RADNUMMER                               
003100     03 MOD-IDARTNR-UT       PIC X(9).                                    
003200*                                 ARTIKELNUMMER                           
003300     03 MOD-IDLEVNR-UT       PIC X(5).                                    
003400*                                 LEVERANTÖRNUMMER                        
003500     03 MOD-BELEVART-UT      PIC X(30).                                   
003600*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
003700     03 MOD-IDARTNR-IN-ATTR  PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-IDARTNR-IN       PIC X(9).                                    
004000*                                 ARTIKELNUMMER                           
004100     03 MOD-IDLEVNR-IN-ATTR  PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-IDLEVNR-IN       PIC X(5).                                    
004400*                                 LEVERANTÖRNUMMER                        
004500     03 MOD-BELEVART-IN-ATTR PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-BELEVART-IN      PIC X(30).                                   
004800*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
004900     03 MOD-BEART-UT         PIC X(25).                                   
005000*                                 ARTIKELBENÄMNING                        
005100     03 MOD-KDBENHOM-UT      PIC 9.                                       
005200*                                 HOMONYMKOD                              
005300     03 MOD-BEART-IN-ATTR    PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-BEART-IN         PIC X(25).                                   
005600*                                 ARTIKELBENÄMNING                        
005700     03 MOD-KDBENHOM-IN-ATTR PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-KDBENHOM-IN      PIC 9.                                       
006000*                                 HOMONYMKOD                              
006100     03 MOD-IDLEVNR          PIC X(5).                                    
006200*                                 LEVERANTÖRNUMMER                        
006300     03 MOD-BELEVART         PIC X(30).                                   
006400*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
006500     03 MOD-TIREGDAT         PIC 9(6).                                    
006600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006700     03 MOD-REANTPSA-UT      PIC Z9.9(3).                                 
006800*                                 ANTAL PER SATS                          
006900     03 MOD-IDSTRTYP-UT      PIC X.                                       
007000*                                 STRUKTURTYP                             
007100     03 MOD-KDSORT-UT        PIC X(2).                                    
007200*                                 SORT-KOD                                
007300     03 MOD-REANTPSA-IN-ATTR PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500     03 MOD-REANTPSA-IN      PIC Z9.9(3).                                 
007600*                                 ANTAL PER SATS                          
007700     03 MOD-IDSTRTYP-IN-ATTR PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900     03 MOD-IDSTRTYP-IN      PIC X.                                       
008000*                                 STRUKTURTYP                             
008100     03 MOD-KDSORT-IN-ATTR   PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300     03 MOD-KDSORT-IN        PIC X(2).                                    
008400*                                 SORT-KOD                                
008500     03 MOD-IDAO-IN-ATTR     PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700     03 MOD-IDAO-IN          PIC X(10).                                   
008800*                                 ÄNDRINGSORDERNUMMER                     
008900     03 MOD-TIAAVV-IN-ATTR   PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100     03 MOD-TIAAVV-IN        PIC 9(4).                                    
009200*                                 ÅR - VECKA  (ÅÅVV)                      
009300     03 MOD-TESTRNOT-GRUPP   OCCURS 2 TIMES.                              
009400*                                                                         
009500        05 MOD-TESTRNOT-IN-ATTR                                           
009600                             PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800        05 MOD-TESTRNOT-IN   PIC X(70).                                   
009900*                                 STRUKTURNOTERING                        
010000     03 MOD-KLAR-IN-ATTR     PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200     03 MOD-KLAR-IN          PIC X.                                       
010300*                                 ALLMÄN SVARSFLAGGA                      
010400     03 MOD-BORT-IN-ATTR     PIC X(2).                                    
010500*                                 MFS ATTRIBUTFÄLT                        
010600     03 MOD-BORT-IN          PIC X.                                       
010700*                                 ALLMÄN SVARSFLAGGA                      
010800     03 MOD-TEMFSINF         PIC X(55).                                   
010900*                                 INFORMATIONSMEDDELANDE                  
011000*** END OF VILMAII-COPY LENGTH= 530 BYTES                                 
