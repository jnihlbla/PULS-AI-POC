000100 01  MOD-W2O30201-CTX.                                                    
000200*                                 MODCOPYTEXT TILL W20302.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDORDNSB-IN      PIC X(4).                                    
000800*                                 SATSORDERNUMMER-BAS                     
000900     03 MOD-IDORDNSS-IN      PIC X.                                       
001000*                                 SATSORDERNUMMER-SUFFIX                  
001100     03 MOD-IDANSK-FOM-IN    PIC X(3).                                    
001200*                                 ANSKAFFARNUMMER                         
001300     03 MOD-IDANSK-TOM-IN    PIC X(3).                                    
001400*                                 ANSKAFFARNUMMER                         
001500     03 MOD-FLBYGGB-IN       PIC X.                                       
001600*                                 FLAGGA BYGGBAR SATSORDER                
001700     03 MOD-IDARTNR-IN       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-ING-IDARTNR-IN   PIC X(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MOD-KDSATKMB-IN      PIC X.                                       
002200*                                 KOMBINATIONSKOD SATS                    
002300     03 MOD-IDORDNSB-UT      PIC X(4).                                    
002400*                                 SATSORDERNUMMER-BAS                     
002500     03 MOD-IDORDNSS-UT      PIC X.                                       
002600*                                 SATSORDERNUMMER-SUFFIX                  
002700     03 MOD-IDANSK-FOM-UT    PIC X(3).                                    
002800*                                 ANSKAFFARNUMMER                         
002900     03 MOD-IDANSK-TOM-UT    PIC X(3).                                    
003000*                                 ANSKAFFARNUMMER                         
003100     03 MOD-FLBYGGB-UT       PIC X.                                       
003200*                                 FLAGGA BYGGBAR SATSORDER                
003300     03 MOD-IDARTNR-UT       PIC X(9).                                    
003400*                                 ARTIKELNUMMER                           
003500     03 MOD-ING-IDARTNR-UT   PIC X(9).                                    
003600*                                 ARTIKELNUMMER                           
003700     03 MOD-KDSATKMB-UT      PIC X.                                       
003800*                                 KOMBINATIONSKOD SATS                    
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
005300     03 MOD-BEART            PIC X(25).                                   
005400*                                 ARTIKELBENÄMNING                        
005500     03 MOD-KVBEART          PIC Z(7).                                    
005600*                                 BESTÄLLT ANTAL STYCKEN                  
005700     03 MOD-FLBYGGB          PIC X.                                       
005800*                                 FLAGGA BYGGBAR SATSORDER                
005900     03 MOD-KVBYGGB          PIC Z(6)9.                                   
006000*                                 ANTAL BYGGBARA SATSER                   
006100     03 MOD-HUV-KDCLAGER     OCCURS 2 TIMES                               
006200                             PIC 9.                                       
006300*                                 CENTRALLAGERKOD                         
006400     03 MOD-KVPB-SATS        OCCURS 2 TIMES                               
006500                             PIC Z(5)9.9.                                 
006600*                                 SATS-PERIODBEHOV                        
006700     03 MOD-KVROS            OCCURS 2 TIMES                               
006800                             PIC Z(6)9.                                   
006900*                                 RESTORDERSALDO                          
007000     03 MOD-KVDISP           OCCURS 2 TIMES                               
007100                             PIC -(7)9.                                   
007200*                                 DISPONIBELT LAGER                       
007300     03 MOD-KVSLAGER         OCCURS 2 TIMES                               
007400                             PIC Z(6)9.                                   
007500*                                 SÄKERHETSLAGER                          
007600     03 MOD-FLSATSPR-HUV     PIC X.                                       
007700*                                 FLAGGA SPÄRRAD SATS EL SATSRAD          
007800     03 MOD-FLSATPRI         PIC X.                                       
007900*                                 MANUELL PRIORITERING AV SATS            
008000     03 MOD-ING-IDARTNR-RAD  OCCURS 7 TIMES                               
008100                             PIC X(11).                                   
008200*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
008300     03 MOD-REANTPSA-RAD     OCCURS 7 TIMES                               
008400                             PIC Z9.9(3).                                 
008500*                                 ANTAL PER SATS                          
008600     03 MOD-REBEART-RAD      OCCURS 7 TIMES                               
008700                             PIC Z(6)9.                                   
008800*                                 ANTAL PER ORDERRAD SATS                 
008900     03 MOD-KVSATRES-RAD     OCCURS 7 TIMES                               
009000                             PIC Z(6)9.                                   
009100*                                 RESERVERAT ANTAL ARTIKLAR SATS          
009200     03 MOD-KVSATROS-RAD     OCCURS 7 TIMES                               
009300                             PIC Z(6)9.                                   
009400*                                 RESTNOTERAT ANTAL ARTIKLAR SATS         
009500     03 MOD-IDANSK-RAD       OCCURS 7 TIMES                               
009600                             PIC Z(2)9.                                   
009700*                                 ANSKAFFARNUMMER                         
009800     03 MOD-FLSATSPR-RAD     OCCURS 7 TIMES                               
009900                             PIC X.                                       
010000*                                 FLAGGA SPÄRRAD SATS EL SATSRAD          
010100     03 MOD-TIDISPIN-RAD     OCCURS 7 TIMES                               
010200                             PIC 9(6).                                    
010300*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
010400     03 MOD-W2O30201-001-GRP.                                             
010500*                                 UPDATE                                  
010600        05 MOD-KVDELA-UPDATE-ATTR                                         
010700                             PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900        05 MOD-KVDELA-UPDATE PIC X(6).                                    
011000*                                 ANTAL BYGGBARA SATSER                   
011100        05 MOD-FLANNULL-REST-UPDATE-ATTR                                  
011200                             PIC X(2).                                    
011300*                                 MFS ATTRIBUTFÄLT                        
011400        05 MOD-FLANNULL-REST-UPDATE                                       
011500                             PIC X.                                       
011600*                                 ANNULLATION                             
011700        05 MOD-FLANNULL-HELA-UPDATE-ATTR                                  
011800                             PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000        05 MOD-FLANNULL-HELA-UPDATE                                       
012100                             PIC X.                                       
012200*                                 ANNULLATION                             
012300        05 MOD-FLSATPRI-UPDATE-ATTR                                       
012400                             PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600        05 MOD-FLSATPRI-UPDATE                                            
012700                             PIC X.                                       
012800*                                 MANUELL PRIORITERING AV SATS            
012900     03 MOD-TEMFSINF         PIC X(55).                                   
013000*                                 INFORMATIONSMEDDELANDE                  
013100*** END OF VILMAII-COPY LENGTH= 652 BYTES                                 
