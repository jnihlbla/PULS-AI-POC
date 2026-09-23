000100 01  DOC-WL01731.                                                         
000200*                                 COPYTEXT FOR INVENTORY REQUEST          
000300*                                 1 LDC                                   
000400     03 DOC-IDAFPRCD         PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 DOC-IDDC             PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 DOC-KDINVKAT         PIC Z9.                                      
000900*                                 INVENTERINGSKATEGORI                    
001000     03 DOC-TIREGDAT         PIC X(6).                                    
001100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001200     03 DOC-TIUTSKR          PIC 9(6).                                    
001300*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
001400     03 DOC-TIUTSTID         PIC 9(6).                                    
001500*                                 UTSKRIFTSTID (TTMMSS)                   
001600     03 DOC-IDPRINTINV       PIC X(6).                                    
001700     03 DOC-IDARTNR          PIC Z(7)9.                                   
001800*                                 ARTIKELNUMMER                           
001900     03 DOC-KDPRODSL         PIC Z9.                                      
002000*                                 PRODUKTSLAG                             
002100     03 DOC-KVUTRS           PIC -(7)9.                                   
002200*                                 UTREDNINGSSALDO                         
002300     03 DOC-BEART            PIC X(25).                                   
002400*                                 ARTIKELBENÄMNING                        
002500     03 DOC-KVAKS            PIC -(7)9.                                   
002600*                                 ANKOMSTSALDO                            
002700     03 DOC-ADLAGOMR         PIC 9(2).                                    
002800*                                 LAGEROMRÅDE                             
002900     03 DOC-ADGANG           PIC 9(2).                                    
003000*                                 GÅNG                                    
003100     03 DOC-ADPLATS          PIC 9(5).                                    
003200*                                 LAGERPLATSNUMMER                        
003300     03 DOC-KVLS             PIC -(7)9.                                   
003400*                                 LAGERSALDO                              
003500     03 DOC-KDSORT           PIC X(2).                                    
003600*                                 SORT-KOD                                
003700     03 DOC-VKART            PIC Z(6)9.                                   
003800*                                 ARTIKELVIKT (G)                         
003900     03 DOC-PRARTSTD         PIC Z(6)9.9(2).                              
004000*                                 ARTIKELSTANDARDPRIS                     
004100     03 DOC-TIJUSTDA         PIC 9(6).                                    
004200*                                 JUSTERINGSDATUM                         
004300     03 DOC-KVJUSTKV         PIC -(6)9.                                   
004400*                                 JUSTERAD KVANTITET                      
004500     03 DOC-KDJUSTYP         PIC 9.                                       
004600*                                 JUSTERINGSTYP                           
004700     03 DOC-KDJUSTYP-A       PIC X.                                       
004800     03 DOC-BETEXT           PIC X(9).                                    
004900*                                 AVIKELSEKOMMENTAR    BETEXT-009         
005000     03 DOC-KVROS            PIC -(7)9.                                   
005100*                                 RESTORDERSALDO                          
005200     03 DOC-TEINVANM         PIC X(25).                                   
005300*                                 INVENTERINGSANMÄRKNING                  
005400     03 DOC-ADBUFFOMR        OCCURS 4 TIMES                               
005500                             PIC Z9.                                      
005600*                                 BUFFERTOMRÅDE                           
005700     03 DOC-ADBUFFGANG       OCCURS 4 TIMES                               
005800                             PIC Z9.                                      
005900*                                 BUFFERT GÅNG                            
006000     03 DOC-ADBUFFPL         OCCURS 4 TIMES                               
006100                             PIC Z(4)9.                                   
006200*                                 BUFFERPLATSNUMMER                       
006300     03 DOC-IDDISTR          OCCURS 7 TIMES                               
006400                             PIC Z(3)9.                                   
006500*                                 DISTRIKTNUMMER                          
006600     03 DOC-IDKUNDNR         OCCURS 7 TIMES                               
006700                             PIC Z(5)9.                                   
006800*                                 KUNDNUMMER                              
006900     03 DOC-IDKUNDRF         OCCURS 7 TIMES                               
007000                             PIC X(10).                                   
007100*                                 KUNDENS REFERENS (ORDERID)              
007200     03 DOC-KVLEVART         OCCURS 7 TIMES                               
007300                             PIC Z(6)9.                                   
007400*                                 LEVERERAT ANTAL STYCK                   
007500     03 DOC-BU-ADBUFFOMR     OCCURS 2 TIMES                               
007600                             PIC Z9.                                      
007700*                                 BUFFERTOMRÅDE                           
007800     03 DOC-BU-ADBUFFGANG    OCCURS 2 TIMES                               
007900                             PIC Z9.                                      
008000*                                 BUFFERT GÅNG                            
008100     03 DOC-BU-ADBUFFPL      OCCURS 2 TIMES                               
008200                             PIC Z(4)9.                                   
008300*                                 BUFFERPLATSNUMMER                       
008400*** END OF VILMAII-COPY LENGTH= 425 BYTES                                 
