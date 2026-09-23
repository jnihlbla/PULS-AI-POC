000100 01  XDCA-W411XDCA.                                                       
000200*                                 LINKAGE AREA FOR W411XDCA -             
000300*                                 PRELIMINARY BOOKING                     
000400*                                 LDC/SDC/NDC ORDER LINE                  
000500*                                                                         
000600     03 XDCA-INPUT.                                                       
000700        05 XDCA-IDDC-CLEAR-IN                                             
000800                             OCCURS 99 TIMES                              
000900                             PIC X(2).                                    
001000*                                 LAGERPRIORITERING VID                   
001100*                                 ORDERCLEARING                           
001200        05 XDCA-FLLDCKND     PIC X.                                       
001300*                                 FL LDC-KUND                             
001400        05 XDCA-IDARTNR      PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600        05 XDCA-IDDC         PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800        05 XDCA-IDDC-TVS     PIC X(2).                                    
001900*                                 DISTRIBUTIONCENTER                      
002000*                                 TVÅNGSSTYRNING                          
002100        05 XDCA-IDLEVNR      PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300        05 XDCA-KDERS        PIC S9(3)           COMP-3.                  
002400*                                 ERSÄTTNINGSKOD                          
002500        05 XDCA-KDSORT       PIC X(2).                                    
002600*                                 SORT-KOD                                
002700        05 XDCA-KVBEART-Q    PIC S9(7)           COMP-3.                  
002800*                                 BESTÄLLT KVANTANPASSAT ANTAL            
002900        05 XDCA-KVQPACK-1    PIC S9(5)           COMP-3.                  
003000*                                 ANTAL I Q1 FÖRPACKNING                  
003100        05 XDCA-TIREGDAT     PIC S9(7)           COMP-3.                  
003200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003300        05 XDCA-TIREGTID     PIC S9(7)           COMP-3.                  
003400*                                 REGISTRERINGSTID                        
003500        05 XDCA-VKART        PIC S9(7)           COMP-3.                  
003600*                                 ARTIKELVIKT (G)                         
003700        05 XDCA-KDCALL       PIC S9(3)           COMP-3.                  
003800*                                 ANROPSTYP                               
003900     03 XDCA-OUTPUT.                                                      
004000        05 XDCA-IDDC-OUT     PIC X(2).                                    
004100*                                 IDENTIFIERARE LAGER                     
004200        05 XDCA-IDDC-RO      PIC X(2).                                    
004300*                                 LAGER DÄR RESTORDER FÅR SKE             
004400        05 XDCA-ADLAGOMR     PIC S9(3)           COMP-3.                  
004500*                                 LAGEROMRÅDE                             
004600        05 XDCA-ADGANG       PIC S9(3)           COMP-3.                  
004700*                                 GÅNG                                    
004800        05 XDCA-ADPLATS      PIC S9(5)           COMP-3.                  
004900*                                 LAGERPLATSNUMMER                        
005000        05 XDCA-KDARTURS     PIC X(2).                                    
005100*                                 ARTIKELURSPRUNGSKOD                     
005200        05 XDCA-KDOI         PIC X(2).                                    
005300*                                 ORDERINGÅNGSTYP                         
005400        05 XDCA-KDORDBEK     PIC 9(2).                                    
005500*                                 ORDERBEKRÄFTELSEKOD                     
005600        05 XDCA-KVPREAVB     PIC S9(7)           COMP-3.                  
005700*                                 PREL-AVB KVANT                          
005800        05 XDCA-KVPRERO      PIC S9(7)           COMP-3.                  
005900*                                 PRELIMINÄR RO-KVANT                     
006000        05 XDCA-TIREGDAT-OUT PIC S9(7)           COMP-3.                  
006100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006200        05 XDCA-TIREGTID-OUT PIC S9(7)           COMP-3.                  
006300*                                 REGISTRERINGSTID                        
006400        05 XDCA-VKART-OUT    PIC S9(7)           COMP-3.                  
006500*                                 ARTIKELVIKT (G)                         
006600        05 XDCA-VKART-NTO    PIC S9(9)           COMP-3.                  
006700*                                 ARTIKELNS NETTOVIKT                     
006800        05 XDCA-VLARTNTO     PIC S9(8)V9(1)      COMP-3.                  
006900*                                 ARTIKELVOLYM (CM3)                      
007000        05 XDCA-DAPUBL       PIC 9(8).                                    
007100*                                 PUBLICERINGSDATUM PER ART/LAND          
007200        05 XDCA-CLEARGROUP.                                               
007300*                                 CLEARINGAREA FÖR ORDERINGÅNG            
007400           07 XDCA-CLEARAREA OCCURS 7 TIMES.                              
007500*                                 CLEARINGAREA FÖR ORDERINGÅNG            
007600              09 XDCA-IDDC-CLEAR                                          
007700                             PIC X(2).                                    
007800*                                 LAGERPRIORITERING VID                   
007900*                                 ORDERCLEARING                           
008000              09 XDCA-FLLF   PIC X.                                       
008100*                                 ARTIKEL LAGERFÖRES                      
008200              09 XDCA-FLCLEAR                                             
008300                             PIC X.                                       
008400*                                 ORDERRAD CLEAR FLAGGA                   
008500     03 XDCA-KVOKS-DAG       PIC S9(7)           COMP-3.                  
008600*                                 ORDERKÖSALDO, KLASS 1                   
008700     03 XDCA-KVOKS-BULK      PIC S9(7)           COMP-3.                  
008800*                                 ORDERKÖSALDO, KLASS 2-4                 
008900*** END OF VILMAII-COPY LENGTH= 329 BYTES                                 
