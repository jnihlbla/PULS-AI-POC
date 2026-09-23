000100 01  RESP-W30183O1.                                                       
000200*                                 COPYTEXT FÖR RESP                       
000300*                                 W30183O1                                
000400     03 RESP-IDBYTKOL-START  PIC 9(3).                                    
000500*                                 BYTES KOLLINUMMER                       
000600     03 RESP-IDARTNR-OBJ-START                                            
000700                             PIC X(9).                                    
000800*                                 OBJEKTNUMMER                            
000900     03 RESP-IDDISTR-START   PIC 9(5).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 RESP-IDBYTKOL-NEXT   PIC 9(3).                                    
001200*                                 BYTES KOLLINUMMER                       
001300     03 RESP-IDARTNR-OBJ-NEXT                                             
001400                             PIC X(9).                                    
001500*                                 OBJEKTNUMMER                            
001600     03 RESP-IDDISTR-NEXT    PIC 9(5).                                    
001700*                                 DISTRIKTNUMMER                          
001800     03 RESP-IDARTNR-OBJ-UPPD-ATTR                                        
001900                             PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 RESP-IDARTNR-OBJ-UPPD                                             
002200                             PIC X(9).                                    
002300*                                 OBJEKTNUMMER                            
002400     03 RESP-KVRETUR-UPPD-ATTR                                            
002500                             PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 RESP-KVRETUR-UPPD    PIC X(7).                                    
002800*                                 ANTAL I RETUR                           
002900     03 RESP-FLBYTKNR-UPPD-ATTR                                           
003000                             PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 RESP-FLBYTKNR-UPPD   PIC X.                                       
003300*                                 KOLLI PROFORMA FLAGGA                   
003400*                                 Y = NYTT KOLLINR                        
003500*                                 N = SAMMA KOLLINR                       
003600     03 RESP-KVLS            PIC Z(6)9.                                   
003700*                                 LAGERSALDO                              
003800     03 RESP-IDBYTFAK        PIC Z(3)9.                                   
003900*                                 BYTES FAKTURANUMMER                     
004000     03 RESP-BORT.                                                        
004100*                                 ARTIKELNUMMER I PROFORMA-FAKTUR         
004200*                                 A                                       
004300        05 RESP-IDARTNR-OBJ-BORT-ATTR                                     
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 RESP-IDARTNR-OBJ-BORT                                          
004700                             PIC X(9).                                    
004800*                                 OBJEKTNUMMER                            
004900        05 RESP-KVRETUR-BORT-ATTR                                         
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 RESP-KVRETUR-BORT PIC X(7).                                    
005300*                                 ANTAL I RETUR                           
005400        05 RESP-IDBYTKOL-BORT-ATTR                                        
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 RESP-IDBYTKOL-BORT                                             
005800                             PIC X(3).                                    
005900*                                 BYTES KOLLINUMMER                       
006000        05 RESP-VKORDBTO-FAKT-UPD-ATTR                                    
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 RESP-VKORDBTO-FAKT-UPD                                         
006400                             PIC X(8).                                    
006500*                                 ORDERVIKT BRUTTO PER FAKTURA            
006600        05 RESP-VKORDBTO-FAKT-UT                                          
006700                             PIC Z(5)9.9.                                 
006800*                                 ORDERVIKT BRUTTO PER FAKTURA            
006900        05 RESP-VLORDBTO-FAKT-UPD-ATTR                                    
007000                             PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200        05 RESP-VLORDBTO-FAKT-UPD                                         
007300                             PIC X(8).                                    
007400*                                 ORDERVOLYM BRUTTO PER FAKTURA           
007500        05 RESP-VLORDBTO-FAKT-UT                                          
007600                             PIC Z(5)9.9.                                 
007700        05 RESP-GODK-FAKT-ATTR                                            
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000        05 RESP-GODK-FAKT    PIC X.                                       
008100     03 RESP-KVRADER         PIC 9(5).                                    
008200*                                 ANTAL RADER                             
008300     03 RESP-PROFORMA-RAD    OCCURS 500 TIMES.                            
008400*                                 ARTIKELNUMMER I PROFORMA-FAKTUR         
008500*                                 A                                       
008600        05 RESP-IDARTNR-OBJ-LINE                                          
008700                             PIC Z(9).                                    
008800*                                 ARTIKELNUMMER                           
008900        05 RESP-KVRETUR-LINE PIC Z(7).                                    
009000*                                 ANTAL I RETUR                           
009100        05 RESP-IDBYTKOL-LINE                                             
009200                             PIC Z(2)9.                                   
009300*                                 BYTES KOLLINUMMER                       
009400        05 RESP-BEART-LINE   PIC X(25).                                   
009500*                                 ARTIKELBENÄMNING                        
009600*** END OF VILMAII-COPY LENGTH= 22137 BYTES                               
