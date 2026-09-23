000100 01  MOD-W90435O1.                                                        
000200*                                 MOD-COPYTEXT FÖR W9043500               
000300*                                 SPIE'S MOTSV. AV W4070200               
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDFTG-IN         PIC X(2).                                    
001100*                                 FÖRETAGSID EKONOM REDOVISNING           
001200     03 MOD-IDARTNR-UT       PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 MOD-IDFTG-UT         PIC X(2).                                    
001500*                                 FÖRETAGSID EKONOM REDOVISNING           
001600     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 MOD-TIGILTIG-FOM-ENTER                                            
001900                             PIC 9(6).                                    
002000*                                 GILTIGHETSDATUM FOM                     
002100     03 MOD-KDANMORS-ENTER   PIC X(2).                                    
002200*                                 ORSAK TILL LEVERANSANMÄRKNING           
002300     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
002400*                                 ARTIKELNUMMER                           
002500     03 MOD-TIGILTIG-FOM-NEXT                                             
002600                             PIC 9(6).                                    
002700*                                 GILTIGHETSDATUM FOM                     
002800     03 MOD-KDANMORS-NEXT    PIC X(2).                                    
002900*                                 ORSAK TILL LEVERANSANMÄRKNING           
003000     03 MOD-RAD-1.                                                        
003100*                                 INFO RAD 1                              
003200        05 MOD-IDARTNR-RAD1  PIC Z(8)9.                                   
003300*                                 ARTIKELNUMMER                           
003400        05 MOD-TIGILTIG-FOM-RAD1                                          
003500                             PIC 9(6).                                    
003600*                                 GILTIGHETSDATUM FOM                     
003700        05 MOD-TIGILTIG-TOM-RAD1                                          
003800                             PIC 9(6).                                    
003900*                                 GILTIGHETSDATUM TOM                     
004000        05 MOD-IDFTG-RAD1    PIC 9(2).                                    
004100*                                 FÖRETAGSID EKONOM REDOVISNING           
004200        05 MOD-KDANMORS-RAD1 PIC X(2).                                    
004300*                                 ORSAK TILL LEVERANSANMÄRKNING           
004400        05 MOD-IDANALYS-RAD1 PIC Z(12).                                   
004500*                                 ANALYSNUMMER                            
004600        05 MOD-IDKONTO-RAD1  PIC Z(10).                                   
004700*                                 KONTO                                   
004800        05 MOD-IDKST-RAD1    PIC X(10).                                   
004900*                                 KOSTNADSSTÄLLE                          
005000        05 MOD-IDUSER-RAD1   PIC X(2).                                    
005100        05 MOD-KVART-RAD1    PIC Z(6)9.                                   
005200*                                 ANTAL ARTNR PER BRYTBEGREPP             
005300     03 MOD-RADER-2-21       OCCURS 20 TIMES.                             
005400*                                 RADINFORMATION                          
005500        05 MOD-IDARTNR       PIC Z(8)9.                                   
005600*                                 ARTIKELNUMMER                           
005700        05 MOD-TIGILTIG-FOM  PIC 9(6).                                    
005800*                                 GILTIGHETSDATUM FOM                     
005900        05 MOD-TIGILTIG-TOM  PIC 9(6).                                    
006000*                                 GILTIGHETSDATUM TOM                     
006100        05 MOD-IDFTG         PIC 9(2).                                    
006200*                                 FÖRETAGSID EKONOM REDOVISNING           
006300        05 MOD-KDANMORS      PIC X(2).                                    
006400*                                 ORSAK TILL LEVERANSANMÄRKNING           
006500        05 MOD-IDANALYS      PIC Z(12).                                   
006600*                                 ANALYSNUMMER                            
006700        05 MOD-IDKONTO       PIC Z(10).                                   
006800*                                 KONTO                                   
006900        05 MOD-IDKST         PIC X(10).                                   
007000*                                 KOSTNADSSTÄLLE                          
007100        05 MOD-IDUSER        PIC X(2).                                    
007200        05 MOD-KVART         PIC Z(6)9.                                   
007300*                                 ANTAL ARTNR PER BRYTBEGREPP             
007400     03 MOD-IDARTNR-UPP-ATTR PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600     03 MOD-IDARTNR-UPP      PIC Z(8)9.                                   
007700*                                 ARTIKELNUMMER                           
007800     03 MOD-TIGILTIG-FOM-UPP-ATTR                                         
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100     03 MOD-TIGILTIG-FOM-UPP PIC 9(6).                                    
008200*                                 GILTIGHETSDATUM FOM                     
008300     03 MOD-TIGILTIG-TOM-UPP-ATTR                                         
008400                             PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600     03 MOD-TIGILTIG-TOM-UPP PIC 9(6).                                    
008700*                                 GILTIGHETSDATUM TOM                     
008800     03 MOD-IDFTG-UPP-ATTR   PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-IDFTG-UPP        PIC 9(2).                                    
009100*                                 FÖRETAGSID EKONOM REDOVISNING           
009200     03 MOD-KDANMORS-UPP-ATTR                                             
009300                             PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-KDANMORS-UPP     PIC X(2).                                    
009600*                                 ORSAK TILL LEVERANSANMÄRKNING           
009700     03 MOD-IDANALYS-UPP-ATTR                                             
009800                             PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000     03 MOD-IDANALYS-UPP     PIC Z(12).                                   
010100*                                 ANALYSNUMMER                            
010200     03 MOD-IDKONTO-UPP-ATTR PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400     03 MOD-IDKONTO-UPP      PIC Z(10).                                   
010500*                                 KONTO                                   
010600     03 MOD-IDKST-UPP-ATTR   PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800     03 MOD-IDKST-UPP        PIC X(10).                                   
010900*                                 KOSTNADSSTÄLLE                          
011000     03 MOD-IDUSER-UPP-ATTR  PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 MOD-IDUSER-UPP       PIC X(2).                                    
011300     03 MOD-FLBORT-UPP-ATTR  PIC X(2).                                    
011400*                                 MFS ATTRIBUTFÄLT                        
011500     03 MOD-FLBORT-UPP       PIC X.                                       
011600*                                 BORTTAGNINGSFLAGGA                      
011700     03 MOD-TEMFSINF         PIC X(55).                                   
011800*                                 INFORMATIONSMEDDELANDE                  
011900*** END OF VILMAII-COPY LENGTH= 1621 BYTES                                
