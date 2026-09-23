000100 01  DIFF-W414XDCD.                                                       
000200*                                 XDC-DIFF LOG                            
000300     03 DIFF-TIREGDAT        PIC 9(6).                                    
000400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000500     03 DIFF-TIKLOCK         PIC Z(7)9.                                   
000600*                                 KLOCKSLAG (TTMMSSTH)                    
000700     03 DIFF-W414XDCA.                                                    
000800*                                 CRAETED IN W40212 IF THE RESULT         
000900*                                 BETWEEN NDCA AND XDCA DIFFERS           
001000        05 DIFF-IDDISTR      PIC Z(3)9.                                   
001100*                                 DISTRIKTNUMMER                          
001200        05 DIFF-IDKUNDNR     PIC Z(5)9.                                   
001300*                                 KUNDNUMMER                              
001400        05 DIFF-IDORDNR5     PIC Z(4)9.                                   
001500*                                 ORDERNUMMER                             
001600        05 DIFF-KDORDKL      PIC 9.                                       
001700*                                 ORDERKLASS                              
001800        05 DIFF-IDARTNR      PIC 9(9).                                    
001900*                                 ARTIKELNUMMER                           
002000        05 DIFF-KVBEART-Q    PIC 9(7).                                    
002100*                                 BESTÄLLT KVANTANPASSAT ANTAL            
002200        05 DIFF-IDDC         PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400        05 DIFF-IDSYSTEM     PIC X(4).                                    
002500*                                 VOLVO VCCS SYSTEMNUMMER                 
002600        05 DIFF-FLSVAR       PIC X.                                       
002700*                                 ALLMÄN SVARSFLAGGA                      
002800        05 DIFF-KVOKS-DAG-NDCA                                            
002900                             PIC -(6)9.                                   
003000*                                 ORDERKÖSALDO, KLASS 1                   
003100        05 DIFF-KVOKS-DAG-XDCA                                            
003200                             PIC -(6)9.                                   
003300*                                 ORDERKÖSALDO, KLASS 1                   
003400        05 DIFF-KVOKS-BULK-NDCA                                           
003500                             PIC -(6)9.                                   
003600*                                 ORDERKÖSALDO, KLASS 2-4                 
003700        05 DIFF-KVOKS-BULK-XDCA                                           
003800                             PIC -(6)9.                                   
003900*                                 ORDERKÖSALDO, KLASS 2-4                 
004000        05 DIFF-NDCA.                                                     
004100           07 DIFF-KDORDBEK-NDCA                                          
004200                             PIC 9(2).                                    
004300*                                 ORDERBEKRÄFTELSEKOD                     
004400           07 DIFF-KVPREAVB-NDCA                                          
004500                             PIC -(6)9.                                   
004600*                                 PREL-AVB KVANT                          
004700           07 DIFF-ADLAGOMR-NDCA                                          
004800                             PIC Z9.                                      
004900*                                 LAGEROMRÅDE                             
005000           07 DIFF-ADGANG-NDCA                                            
005100                             PIC Z9.                                      
005200*                                 GÅNG                                    
005300           07 DIFF-ADPLATS-NDCA                                           
005400                             PIC Z(4)9.                                   
005500*                                 LAGERPLATSNUMMER                        
005600           07 DIFF-IDDC-NDCA PIC X(2).                                    
005700*                                 IDENTIFIERARE LAGER                     
005800           07 DIFF-IDDC-RO-NDCA                                           
005900                             PIC X(2).                                    
006000*                                 LAGER DÄR RESTORDER FÅR SKE             
006100           07 DIFF-KDARTURS-NDCA                                          
006200                             PIC X(2).                                    
006300*                                 ARTIKELURSPRUNGSKOD                     
006400           07 DIFF-KDOI-NDCA PIC X(2).                                    
006500*                                 ORDERINGÅNGSTYP                         
006600           07 DIFF-KVPRERO-NDCA                                           
006700                             PIC -(6)9.                                   
006800*                                 PRELIMINÄR RO-KVANT                     
006900           07 DIFF-VKART-NDCA                                             
007000                             PIC Z(6)9.                                   
007100*                                 ARTIKELVIKT (G)                         
007200           07 DIFF-VKART-NTO-NDCA                                         
007300                             PIC 9(8).                                    
007400*                                 ARTIKELNS NETTOVIKT                     
007500           07 DIFF-VLARTNTO-NDCA                                          
007600                             PIC Z(7)9.9.                                 
007700*                                 ARTIKELVOLYM (CM3)                      
007800           07 DIFF-OI-CLEAR-GRP-NDCA.                                     
007900*                                 CLEARINGAREA FÖR ORDERINGÅNG            
008000              09 DIFF-CLEARAREA                                           
008100                             OCCURS 7 TIMES.                              
008200*                                 CLEARINGAREA FÖR ORDERINGÅNG            
008300                 11 DIFF-IDDC-CLEAR                                       
008400                             PIC X(2).                                    
008500*                                 LAGERPRIORITERING VID                   
008600*                                 ORDERCLEARING                           
008700                 11 DIFF-FLLF                                             
008800                             PIC X.                                       
008900*                                 ARTIKEL LAGERFÖRES                      
009000                 11 DIFF-FLCLEAR                                          
009100                             PIC X.                                       
009200*                                 ORDERRAD CLEAR FLAGGA                   
009300        05 DIFF-XDCA.                                                     
009400           07 DIFF-KDORDBEK-XDCA                                          
009500                             PIC 9(2).                                    
009600*                                 ORDERBEKRÄFTELSEKOD                     
009700           07 DIFF-KVPREAVB-XDCA                                          
009800                             PIC -(6)9.                                   
009900*                                 PREL-AVB KVANT                          
010000           07 DIFF-ADLAGOMR-XDCA                                          
010100                             PIC Z9.                                      
010200*                                 LAGEROMRÅDE                             
010300           07 DIFF-ADGANG-XDCA                                            
010400                             PIC Z9.                                      
010500*                                 GÅNG                                    
010600           07 DIFF-ADPLATS-XDCA                                           
010700                             PIC Z(4)9.                                   
010800*                                 LAGERPLATSNUMMER                        
010900           07 DIFF-IDDC-XDCA PIC X(2).                                    
011000*                                 IDENTIFIERARE LAGER                     
011100           07 DIFF-IDDC-RO-XDCA                                           
011200                             PIC X(2).                                    
011300*                                 LAGER DÄR RESTORDER FÅR SKE             
011400           07 DIFF-KDARTURS-XDCA                                          
011500                             PIC X(2).                                    
011600*                                 ARTIKELURSPRUNGSKOD                     
011700           07 DIFF-KDOI-XDCA PIC X(2).                                    
011800*                                 ORDERINGÅNGSTYP                         
011900           07 DIFF-KVPRERO-XDCA                                           
012000                             PIC -(6)9.                                   
012100*                                 PRELIMINÄR RO-KVANT                     
012200           07 DIFF-VKART-XDCA                                             
012300                             PIC Z(6)9.                                   
012400*                                 ARTIKELVIKT (G)                         
012500           07 DIFF-VKART-NTO-XDCA                                         
012600                             PIC 9(8).                                    
012700*                                 ARTIKELNS NETTOVIKT                     
012800           07 DIFF-VLARTNTO-XDCA                                          
012900                             PIC Z(7)9.9.                                 
013000*                                 ARTIKELVOLYM (CM3)                      
013100           07 DIFF-OI-CLEAR-GRP-XDCA.                                     
013200*                                 CLEARINGAREA FÖR ORDERINGÅNG            
013300              09 DIFF-CLEARAREA                                           
013400                             OCCURS 7 TIMES.                              
013500*                                 CLEARINGAREA FÖR ORDERINGÅNG            
013600                 11 DIFF-IDDC-CLEAR                                       
013700                             PIC X(2).                                    
013800*                                 LAGERPRIORITERING VID                   
013900*                                 ORDERCLEARING                           
014000                 11 DIFF-FLLF                                             
014100                             PIC X.                                       
014200*                                 ARTIKEL LAGERFÖRES                      
014300                 11 DIFF-FLCLEAR                                          
014400                             PIC X.                                       
014500*                                 ORDERRAD CLEAR FLAGGA                   
014600*** END OF VILMAII-COPY LENGTH= 253 BYTES                                 
