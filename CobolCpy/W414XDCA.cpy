000100 01  DIFF-W414XDCA.                                                       
000200*                                 CRAETED IN W40212 IF THE RESULT         
000300*                                 BETWEEN NDCA AND XDCA DIFFERS           
000400     03 DIFF-IDDISTR         PIC Z(3)9.                                   
000500*                                 DISTRIKTNUMMER                          
000600     03 DIFF-IDKUNDNR        PIC Z(5)9.                                   
000700*                                 KUNDNUMMER                              
000800     03 DIFF-IDORDNR5        PIC Z(4)9.                                   
000900*                                 ORDERNUMMER                             
001000     03 DIFF-KDORDKL         PIC 9.                                       
001100*                                 ORDERKLASS                              
001200     03 DIFF-IDARTNR         PIC 9(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 DIFF-KVBEART-Q       PIC 9(7).                                    
001500*                                 BESTÄLLT KVANTANPASSAT ANTAL            
001600     03 DIFF-IDDC            PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 DIFF-IDSYSTEM        PIC X(4).                                    
001900*                                 VOLVO VCCS SYSTEMNUMMER                 
002000     03 DIFF-FLSVAR          PIC X.                                       
002100*                                 ALLMÄN SVARSFLAGGA                      
002200     03 DIFF-KVOKS-DAG-NDCA  PIC -(6)9.                                   
002300*                                 ORDERKÖSALDO, KLASS 1                   
002400     03 DIFF-KVOKS-DAG-XDCA  PIC -(6)9.                                   
002500*                                 ORDERKÖSALDO, KLASS 1                   
002600     03 DIFF-KVOKS-BULK-NDCA PIC -(6)9.                                   
002700*                                 ORDERKÖSALDO, KLASS 2-4                 
002800     03 DIFF-KVOKS-BULK-XDCA PIC -(6)9.                                   
002900*                                 ORDERKÖSALDO, KLASS 2-4                 
003000     03 DIFF-NDCA.                                                        
003100        05 DIFF-KDORDBEK-NDCA                                             
003200                             PIC 9(2).                                    
003300*                                 ORDERBEKRÄFTELSEKOD                     
003400        05 DIFF-KVPREAVB-NDCA                                             
003500                             PIC -(6)9.                                   
003600*                                 PREL-AVB KVANT                          
003700        05 DIFF-ADLAGOMR-NDCA                                             
003800                             PIC Z9.                                      
003900*                                 LAGEROMRÅDE                             
004000        05 DIFF-ADGANG-NDCA  PIC Z9.                                      
004100*                                 GÅNG                                    
004200        05 DIFF-ADPLATS-NDCA PIC Z(4)9.                                   
004300*                                 LAGERPLATSNUMMER                        
004400        05 DIFF-IDDC-NDCA    PIC X(2).                                    
004500*                                 IDENTIFIERARE LAGER                     
004600        05 DIFF-IDDC-RO-NDCA PIC X(2).                                    
004700*                                 LAGER DÄR RESTORDER FÅR SKE             
004800        05 DIFF-KDARTURS-NDCA                                             
004900                             PIC X(2).                                    
005000*                                 ARTIKELURSPRUNGSKOD                     
005100        05 DIFF-KDOI-NDCA    PIC X(2).                                    
005200*                                 ORDERINGÅNGSTYP                         
005300        05 DIFF-KVPRERO-NDCA PIC -(6)9.                                   
005400*                                 PRELIMINÄR RO-KVANT                     
005500        05 DIFF-VKART-NDCA   PIC Z(6)9.                                   
005600*                                 ARTIKELVIKT (G)                         
005700        05 DIFF-VKART-NTO-NDCA                                            
005800                             PIC 9(8).                                    
005900*                                 ARTIKELNS NETTOVIKT                     
006000        05 DIFF-VLARTNTO-NDCA                                             
006100                             PIC Z(7)9.9.                                 
006200*                                 ARTIKELVOLYM (CM3)                      
006300        05 DIFF-OI-CLEAR-GRP-NDCA.                                        
006400*                                 CLEARINGAREA FÖR ORDERINGÅNG            
006500           07 DIFF-CLEARAREA OCCURS 7 TIMES.                              
006600*                                 CLEARINGAREA FÖR ORDERINGÅNG            
006700              09 DIFF-IDDC-CLEAR                                          
006800                             PIC X(2).                                    
006900*                                 LAGERPRIORITERING VID                   
007000*                                 ORDERCLEARING                           
007100              09 DIFF-FLLF   PIC X.                                       
007200*                                 ARTIKEL LAGERFÖRES                      
007300              09 DIFF-FLCLEAR                                             
007400                             PIC X.                                       
007500*                                 ORDERRAD CLEAR FLAGGA                   
007600     03 DIFF-XDCA.                                                        
007700        05 DIFF-KDORDBEK-XDCA                                             
007800                             PIC 9(2).                                    
007900*                                 ORDERBEKRÄFTELSEKOD                     
008000        05 DIFF-KVPREAVB-XDCA                                             
008100                             PIC -(6)9.                                   
008200*                                 PREL-AVB KVANT                          
008300        05 DIFF-ADLAGOMR-XDCA                                             
008400                             PIC Z9.                                      
008500*                                 LAGEROMRÅDE                             
008600        05 DIFF-ADGANG-XDCA  PIC Z9.                                      
008700*                                 GÅNG                                    
008800        05 DIFF-ADPLATS-XDCA PIC Z(4)9.                                   
008900*                                 LAGERPLATSNUMMER                        
009000        05 DIFF-IDDC-XDCA    PIC X(2).                                    
009100*                                 IDENTIFIERARE LAGER                     
009200        05 DIFF-IDDC-RO-XDCA PIC X(2).                                    
009300*                                 LAGER DÄR RESTORDER FÅR SKE             
009400        05 DIFF-KDARTURS-XDCA                                             
009500                             PIC X(2).                                    
009600*                                 ARTIKELURSPRUNGSKOD                     
009700        05 DIFF-KDOI-XDCA    PIC X(2).                                    
009800*                                 ORDERINGÅNGSTYP                         
009900        05 DIFF-KVPRERO-XDCA PIC -(6)9.                                   
010000*                                 PRELIMINÄR RO-KVANT                     
010100        05 DIFF-VKART-XDCA   PIC Z(6)9.                                   
010200*                                 ARTIKELVIKT (G)                         
010300        05 DIFF-VKART-NTO-XDCA                                            
010400                             PIC 9(8).                                    
010500*                                 ARTIKELNS NETTOVIKT                     
010600        05 DIFF-VLARTNTO-XDCA                                             
010700                             PIC Z(7)9.9.                                 
010800*                                 ARTIKELVOLYM (CM3)                      
010900        05 DIFF-OI-CLEAR-GRP-XDCA.                                        
011000*                                 CLEARINGAREA FÖR ORDERINGÅNG            
011100           07 DIFF-CLEARAREA OCCURS 7 TIMES.                              
011200*                                 CLEARINGAREA FÖR ORDERINGÅNG            
011300              09 DIFF-IDDC-CLEAR                                          
011400                             PIC X(2).                                    
011500*                                 LAGERPRIORITERING VID                   
011600*                                 ORDERCLEARING                           
011700              09 DIFF-FLLF   PIC X.                                       
011800*                                 ARTIKEL LAGERFÖRES                      
011900              09 DIFF-FLCLEAR                                             
012000                             PIC X.                                       
012100*                                 ORDERRAD CLEAR FLAGGA                   
012200*** END OF VILMAII-COPY LENGTH= 239 BYTES                                 
