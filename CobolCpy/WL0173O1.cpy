000100 01  RESP-WL0173O1.                                                       
000200*                                 COPYTEXT FOR INVENTORY REQUEST          
000300*                                 1 API                                   
000400     03 RESP-IDDC            PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 RESP-IDARTNR         PIC 9(8).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 RESP-KDINVPRIO       PIC 9.                                       
000900*                                 INVENTERING PRIORITET                   
001000     03 RESP-KDINVKAT        PIC 9(2).                                    
001100*                                 INVENTERINGSKATEGORI                    
001200     03 RESP-DAREGDAT        PIC X(10).                                   
001300*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001400     03 RESP-TIUTSKR         PIC X(10).                                   
001500*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
001600     03 RESP-TIUTSTID        PIC X(8).                                    
001700*                                 UTSKRIFTSTID (TTMMSS)                   
001800     03 RESP-IDPRINTINV      PIC X(6).                                    
001900     03 RESP-KDPRODSL        PIC 9(2).                                    
002000*                                 PRODUKTSLAG                             
002100     03 RESP-KVUTRS          PIC -(7)9.                                   
002200*                                 UTREDNINGSSALDO                         
002300     03 RESP-BEART           PIC X(25).                                   
002400*                                 ARTIKELBENÄMNING                        
002500     03 RESP-KVAKS           PIC -(7)9.                                   
002600*                                 ANKOMSTSALDO                            
002700     03 RESP-ADLAGOMR        PIC X(2).                                    
002800*                                 LAGEROMRÅDE                             
002900     03 RESP-ADGANG          PIC X(2).                                    
003000*                                 GÅNG                                    
003100     03 RESP-ADPLATS         PIC X(5).                                    
003200*                                 LAGERPLATSNUMMER                        
003300     03 RESP-KVLS            PIC -(7)9.                                   
003400*                                 LAGERSALDO                              
003500     03 RESP-KDSORT          PIC X(2).                                    
003600*                                 SORT-KOD                                
003700     03 RESP-VKART           PIC 9(7).                                    
003800*                                 ARTIKELVIKT (G)                         
003900     03 RESP-PRARTSTD        PIC Z(6)9.9(2).                              
004000*                                 ARTIKELSTANDARDPRIS                     
004100     03 RESP-TIJUSTDA        PIC X(10).                                   
004200*                                 JUSTERINGSDATUM                         
004300     03 RESP-KVJUSTKV        PIC -(6)9.                                   
004400*                                 JUSTERAD KVANTITET                      
004500     03 RESP-KDJUSTYP        PIC 9.                                       
004600*                                 JUSTERINGSTYP                           
004700     03 RESP-KDJUSTYP-A      PIC X.                                       
004800     03 RESP-BETEXT          PIC X(9).                                    
004900*                                 AVIKELSEKOMMENTAR    BETEXT-009         
005000     03 RESP-KVROS           PIC -(7)9.                                   
005100*                                 RESTORDERSALDO                          
005200     03 RESP-TEINVANM        PIC X(25).                                   
005300*                                 INVENTERINGSANMÄRKNING                  
005400     03 RESP-KVRADER-BUF     PIC 9(3).                                    
005500*                                 ANTAL RADER                             
005600     03 RESP-BUFFAREA-GRP    OCCURS 4 TIMES.                              
005700*                                 ARTIKEL BUFFERT AREA                    
005800        05 RESP-ADBUFFOMR    PIC X(2).                                    
005900*                                 BUFFERTOMRÅDE                           
006000        05 RESP-ADBUFFGANG   PIC X(2).                                    
006100*                                 BUFFERT GÅNG                            
006200        05 RESP-ADBUFFPL     PIC X(5).                                    
006300*                                 BUFFERPLATSNUMMER                       
006400        05 RESP-KVBUFF-F     PIC -(7)9.                                   
006500*                                 FÖRÄDLAT BUFFERSALDO                    
006600        05 RESP-KVKOLLI-F    PIC 9(4).                                    
006700*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
006800     03 RESP-KVRADER-EFR     PIC 9(3).                                    
006900*                                 ANTAL RADER                             
007000     03 RESP-IDGMTREF-GRP    OCCURS 7 TIMES.                              
007100*                                 IDGMTREF                                
007200        05 RESP-IDDISTR      PIC 9(4).                                    
007300*                                 DISTRIKTNUMMER                          
007400        05 RESP-IDKUNDNR     PIC 9(6).                                    
007500*                                 KUNDNUMMER                              
007600        05 RESP-IDKUNDRF     PIC X(10).                                   
007700*                                 KUNDENS REFERENS (ORDERID)              
007800        05 RESP-KVLEVART     PIC 9(7).                                    
007900*                                 LEVERERAT ANTAL STYCK                   
008000*** END OF VILMAII-COPY LENGTH= 466 BYTES                                 
