000100 01  MOD-W2O30301.                                                        
000200*                                 MODCOPYTEXT TILL W20303.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDORDNSB-IN      PIC X(4).                                    
000800*                                 SATSORDERNUMMER-BAS                     
000900     03 MOD-IDORDNSS-IN      PIC X.                                       
001000*                                 SATSORDERNUMMER-SUFFIX                  
001100     03 MOD-ING-IDARTNR-IN   PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-KDSATKMB-IN      PIC X.                                       
001400*                                 KOMBINATIONSKOD SATS                    
001500     03 MOD-IDANSK-FOM-IN    PIC X(3).                                    
001600*                                 ANSKAFFARNUMMER                         
001700     03 MOD-IDANSK-TOM-IN    PIC X(3).                                    
001800*                                 ANSKAFFARNUMMER                         
001900     03 MOD-FLBYGGB-IN       PIC X.                                       
002000*                                 FLAGGA BYGGBAR SATSORDER                
002100     03 MOD-IDARTNR-IN       PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-IDORDNSB-UT      PIC X(4).                                    
002400*                                 SATSORDERNUMMER-BAS                     
002500     03 MOD-IDORDNSS-UT      PIC X.                                       
002600*                                 SATSORDERNUMMER-SUFFIX                  
002700     03 MOD-ING-IDARTNR-UT   PIC X(9).                                    
002800*                                 ARTIKELNUMMER                           
002900     03 MOD-KDSATKMB-UT      PIC X.                                       
003000*                                 KOMBINATIONSKOD SATS                    
003100     03 MOD-IDANSK-FOM-UT    PIC X(3).                                    
003200*                                 ANSKAFFARNUMMER                         
003300     03 MOD-IDANSK-TOM-UT    PIC X(3).                                    
003400*                                 ANSKAFFARNUMMER                         
003500     03 MOD-FLBYGGB-UT       PIC X.                                       
003600*                                 FLAGGA BYGGBAR SATSORDER                
003700     03 MOD-IDARTNR-UT       PIC X(9).                                    
003800*                                 ARTIKELNUMMER                           
003900     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
004000*                                 ARTIKELNUMMER                           
004100     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
004200*                                 ARTIKELNUMMER                           
004300     03 MOD-IDARTNR          PIC X(11).                                   
004400*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
004500     03 MOD-KDCLAGER         PIC 9.                                       
004600*                                 CENTRALLAGERKOD                         
004700     03 MOD-IDPRC.                                                        
004800*                                 PRODUKTIONSKANAL                        
004900        05 MOD-IDPRCBAS      PIC X(3).                                    
005000*                                 PRC-BAS                                 
005100        05 MOD-IDPRCVAR      PIC X.                                       
005200*                                 PRC-VARIANT                             
005300     03 MOD-FLSATPRI         PIC X.                                       
005400*                                 MANUELL PRIORITERING AV SATS            
005500     03 MOD-FLBYGGB          PIC X.                                       
005600*                                 FLAGGA BYGGBAR SATSORDER                
005700     03 MOD-KDSATKMB         PIC X.                                       
005800*                                 KOMBINATIONSKOD SATS                    
005900     03 MOD-IDPRODNR         PIC Z(6)9.                                   
006000*                                 PRODUKTIONSNUMMER                       
006100     03 MOD-BEART            PIC X(25).                                   
006200*                                 ARTIKELBENÄMNING                        
006300     03 MOD-KVBEART          PIC Z(7).                                    
006400*                                 BESTÄLLT ANTAL ARTIKLAR                 
006500     03 MOD-KVBYGGB          PIC Z(6)9.                                   
006600*                                 ANTAL BYGGBARA SATSER                   
006700     03 MOD-ING-IDARTNR-RAD  OCCURS 8 TIMES                               
006800                             PIC X(11).                                   
006900*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
007000     03 MOD-REANTPSA-RAD     OCCURS 8 TIMES                               
007100                             PIC Z9.9(3).                                 
007200*                                 ANTAL PER SATS                          
007300     03 MOD-REBEART-RAD      OCCURS 8 TIMES                               
007400                             PIC Z(6)9.                                   
007500*                                 ANTAL PER ORDERRAD SATS                 
007600     03 MOD-KVSATRES-RAD     OCCURS 8 TIMES                               
007700                             PIC Z(6)9.                                   
007800*                                 RESERVERAT ANTAL ARTIKLAR SATS          
007900     03 MOD-KVSATROS-RAD     OCCURS 8 TIMES                               
008000                             PIC Z(6)9.                                   
008100*                                 RESTNOTERAT ANTAL ARTIKLAR SATS         
008200     03 MOD-KDSATAND-RAD     OCCURS 8 TIMES                               
008300                             PIC X.                                       
008400*                                 ÄNDRINGSKOD SATS                        
008500     03 MOD-FLSATRAS-RAD     OCCURS 8 TIMES                               
008600                             PIC X.                                       
008700*                                 FLAGGA FLER FINNS I RASA  SATS          
008800     03 MOD-KDSATKMB-RAD     OCCURS 8 TIMES                               
008900                             PIC X.                                       
009000*                                 KOMBINATIONSKOD SATS                    
009100     03 MOD-FLSATSPR-RAD     OCCURS 8 TIMES                               
009200                             PIC X.                                       
009300*                                 FLAGGA SPÄRRAD SATS EL SATSRAD          
009400     03 MOD-TIDISPIN-RAD     OCCURS 8 TIMES                               
009500                             PIC 9(6).                                    
009600*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
009700     03 MOD-UPDATE.                                                       
009800*                                 UPDATE                                  
009900        05 MOD-ING-IDARTNR1-UPDATE-ATTR                                   
010000                             PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200        05 MOD-ING-IDARTNR1-UPDATE                                        
010300                             PIC X(9).                                    
010400*                                 ARTIKELNUMMER                           
010500        05 MOD-REANTPSA1-UPDATE-ATTR                                      
010600                             PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800        05 MOD-REANTPSA1-UPDATE                                           
010900                             PIC X(6).                                    
011000*                                 ANTAL PER SATS                          
011100        05 MOD-REBEART1-UPDATE-ATTR                                       
011200                             PIC X(2).                                    
011300*                                 MFS ATTRIBUTFÄLT                        
011400        05 MOD-REBEART1-UPDATE                                            
011500                             PIC X(7).                                    
011600*                                 ANTAL PER ORDERRAD SATS                 
011700        05 MOD-KDSATAND1-UPDATE-ATTR                                      
011800                             PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000        05 MOD-KDSATAND1-UPDATE                                           
012100                             PIC X.                                       
012200*                                 ÄNDRINGSKOD SATS                        
012300        05 MOD-KDSATKMB-UPDATE-ATTR                                       
012400                             PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600        05 MOD-KDSATKMB-UPDATE                                            
012700                             PIC X.                                       
012800*                                 KOMBINATIONSKOD SATS                    
012900        05 MOD-ING-IDARTNR2-UPDATE-ATTR                                   
013000                             PIC X(2).                                    
013100*                                 MFS ATTRIBUTFÄLT                        
013200        05 MOD-ING-IDARTNR2-UPDATE                                        
013300                             PIC X(9).                                    
013400*                                 ARTIKELNUMMER                           
013500        05 MOD-REANTPSA2-UPDATE-ATTR                                      
013600                             PIC X(2).                                    
013700*                                 MFS ATTRIBUTFÄLT                        
013800        05 MOD-REANTPSA2-UPDATE                                           
013900                             PIC X(6).                                    
014000*                                 ANTAL PER SATS                          
014100        05 MOD-REBEART2-UPDATE-ATTR                                       
014200                             PIC X(2).                                    
014300*                                 MFS ATTRIBUTFÄLT                        
014400        05 MOD-REBEART2-UPDATE                                            
014500                             PIC X(7).                                    
014600*                                 ANTAL PER ORDERRAD SATS                 
014700        05 MOD-KDSATAND2-UPDATE-ATTR                                      
014800                             PIC X(2).                                    
014900*                                 MFS ATTRIBUTFÄLT                        
015000        05 MOD-KDSATAND2-UPDATE                                           
015100                             PIC X.                                       
015200*                                 ÄNDRINGSKOD SATS                        
015300     03 MOD-TEMFSINF         PIC X(61).                                   
015400*                                 INFORMATIONSMEDDELANDE                  
015500*** END COPY W2O30301    LENGTH=699                                       
