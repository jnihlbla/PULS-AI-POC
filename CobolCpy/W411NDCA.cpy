000100 01  NDCA-W411NDCA.                                                       
000200*                                 LÄNKAREA TILL W411NDCA -                
000300*                                 KONTROLL AV PRELIMINÄRBOKNING           
000400*                                 AV EN NDC-RAD                           
000500     03 NDCA-ADLAGOMR        PIC S9(3)           COMP-3.                  
000600*                                 LAGEROMRÅDE                             
000700     03 NDCA-ADGANG          PIC S9(3)           COMP-3.                  
000800*                                 GÅNG                                    
000900     03 NDCA-ADPLATS         PIC S9(5)           COMP-3.                  
001000*                                 LAGERPLATSNUMMER                        
001100     03 NDCA-CLEARGROUP.                                                  
001200*                                 CLEARINGAREA FÖR ORDERINGÅNG            
001300        05 NDCA-CLEARAREA    OCCURS 7 TIMES.                              
001400*                                 CLEARINGAREA FÖR ORDERINGÅNG            
001500           07 NDCA-IDDC-CLEAR                                             
001600                             PIC X(2).                                    
001700*                                 LAGERPRIORITERING VID                   
001800*                                 ORDERCLEARING                           
001900           07 NDCA-FLLF      PIC X.                                       
002000*                                 ARTIKEL LAGERFÖRES                      
002100           07 NDCA-FLCLEAR   PIC X.                                       
002200*                                 ORDERRAD CLEAR FLAGGA                   
002300     03 NDCA-FLFORBI         PIC X.                                       
002400*                                 FÖRBIORDERFLAGGA                        
002500     03 NDCA-FLLDCKND        PIC X.                                       
002600*                                 FL LDC-KUND                             
002700     03 NDCA-FLORDSPE        PIC X.                                       
002800*                                 SPECIALORDERFLAGGA                      
002900     03 NDCA-FLPRELRO        PIC X.                                       
003000*                                 PRELIMINÄR RESTORDERFLAGGA              
003100     03 NDCA-FLRESTN         PIC X.                                       
003200*                                 RESTNOTERING ?                          
003300     03 NDCA-IDARTNR         PIC S9(9)           COMP-3.                  
003400*                                 ARTIKELNUMMER                           
003500     03 NDCA-IDDC            PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700     03 NDCA-IDDC-CLEAR-GRP.                                              
003800*                                 GRUPP AV IDDC-CLEAR                     
003900        05 NDCA-IDDC-CLEAR   OCCURS 7 TIMES                               
004000                             PIC X(2).                                    
004100*                                 LAGERPRIORITERING VID                   
004200*                                 ORDERCLEARING                           
004300     03 NDCA-IDDC-RO         PIC X(2).                                    
004400*                                 LAGER DÄR RESTORDER FÅR SKE             
004500     03 NDCA-IDDC-TVS        PIC X(2).                                    
004600*                                 DISTRIBUTIONCENTER                      
004700*                                 TVÅNGSSTYRNING                          
004800     03 NDCA-IDDISTR         PIC S9(5)           COMP-3.                  
004900*                                 DISTRIKTNUMMER                          
005000     03 NDCA-IXDCCLEAR       PIC 9.                                       
005100*                                 CLEARING DC SEKVENS                     
005200     03 NDCA-KDARTURS        PIC X(2).                                    
005300*                                 ARTIKELURSPRUNGSKOD                     
005400     03 NDCA-KDERS           PIC S9(3)           COMP-3.                  
005500*                                 ERSÄTTNINGSKOD                          
005600     03 NDCA-KDOI            PIC X(2).                                    
005700*                                 ORDERINGÅNGSTYP                         
005800     03 NDCA-KDORDBEK        PIC 9(2).                                    
005900*                                 ORDERBEKRÄFTELSEKOD                     
006000     03 NDCA-KDORDING        PIC S9              COMP-3.                  
006100*                                 UPPDATERING ORDERINGÅNG                 
006200     03 NDCA-KDORDKL         PIC S9              COMP-3.                  
006300*                                 ORDERKLASS                              
006400     03 NDCA-KDSORT          PIC X(2).                                    
006500*                                 SORT-KOD                                
006600     03 NDCA-KVBEART-Q       PIC S9(7)           COMP-3.                  
006700*                                 BESTÄLLT KVANTANPASSAT ANTAL            
006800     03 NDCA-KVQPACK-1       PIC S9(5)           COMP-3.                  
006900*                                 ANTAL I Q1 FÖRPACKNING                  
007000     03 NDCA-KVDAGAR-DOW     PIC S9(3)           COMP-3.                  
007100*                                 ANTAL DAGAR FÖRE DC CLEARING            
007200     03 NDCA-KVPREAVB        PIC S9(7)           COMP-3.                  
007300*                                 PREL-AVB KVANT                          
007400     03 NDCA-KVPRERO         PIC S9(7)           COMP-3.                  
007500*                                 PRELIMINÄR RO-KVANT                     
007600     03 NDCA-TIREGDAT        PIC S9(7)           COMP-3.                  
007700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007800     03 NDCA-TIREGTID        PIC S9(7)           COMP-3.                  
007900*                                 REGISTRERINGSTID                        
008000     03 NDCA-VKART           PIC S9(7)           COMP-3.                  
008100*                                 ARTIKELVIKT (G)                         
008200     03 NDCA-VKART-NTO       PIC S9(9)           COMP-3.                  
008300*                                 ARTIKELNS NETTOVIKT                     
008400     03 NDCA-VLARTNTO        PIC S9(8)V9(1)      COMP-3.                  
008500*                                 ARTIKELVOLYM (CM3)                      
008600     03 NDCA-KDCALL          PIC S9(3)           COMP-3.                  
008700*                                 ANROPSTYP                               
008800     03 NDCA-DAPUBL          PIC 9(8).                                    
008900*                                 PUBLICERINGSDATUM PER ART/LAND          
009000*** END OF VILMAII-COPY LENGTH= 130 BYTES                                 
