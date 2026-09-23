000100 01  W2O43301.                                                            
000200*                                 COPYTEXT FÖR MOD W2O43301               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 ARTIKEL-UT.                                                       
001000        05 IDARTNR-UT        PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200        05 STRECK-1          PIC X.                                       
001300        05 REKSIFFR          PIC 9.                                       
001400*                                 KONTROLLSIFFRA                          
001500     03 IDDC-IN              PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 IDDC-UT              PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 BEART-ENG            PIC X(25).                                   
002000*                                 ENGELSK ARTIKELBENÄMNING                
002100     03 TIFINLV              PIC 9(5).                                    
002200*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
002300     03 IDBERED              PIC Z9.                                      
002400*                                 BEREDARENUMMER                          
002500     03 TEORSAK              PIC X(50).                                   
002600*                                 INFO OM SLAG AV ÅTGÄRD                  
002700     03 IDAO                 PIC X(10).                                   
002800*                                 ÄNDRINGSORDERNUMMER                     
002900     03 IDKAT                OCCURS 3 TIMES                               
003000                             PIC X(5).                                    
003100*                                 KATALOGBETECKNING                       
003200     03 IDPROENH             OCCURS 2 TIMES                               
003300                             PIC X(8).                                    
003400*                                 PRODUKTIONSENHET                        
003500     03 IDPROJ               PIC X(4).                                    
003600*                                 PARTS PROJEKTIDENTITET                  
003700     03 TEARTNOT             PIC X(40).                                   
003800*                                 ARTIKEL NOTERING                        
003900     03 IDPROJK              PIC X(4).                                    
004000*                                 PROJEKTIDENTITET KONSTRUKTION           
004100     03 TEARTNOT-PP          PIC X(40).                                   
004200*                                 ARTIKEL NOTERING                        
004300     03 FLBYTES              PIC X.                                       
004400*                                 BYTESARTIKEL                            
004500     03 IDARTNR-TILLK        PIC Z(7)9.                                   
004600*                                 TILLKOMMANDE ARTIKELNUMMER              
004700     03 KDERS                PIC Z9.                                      
004800*                                 ERSÄTTNINGSKOD                          
004900     03 MFL                  PIC X(2).                                    
005000     03 IDARTNR-MOTSV        PIC Z(7)9.                                   
005100*                                 MOTSVARANDE ARTIKEL                     
005200     03 KVARTVAGN            PIC Z(2)9.                                   
005300*                                 ANTAL ARTIKLAR PER VAGN                 
005400     03 IDLEVNR              PIC X(5).                                    
005500*                                 LEVERANTÖRNUMMER                        
005600     03 PRARTSTD             PIC Z(6)9.9(2).                              
005700*                                 ARTIKELSTANDARDPRIS                     
005800     03 IDANSK-CDC           PIC Z(2)9.                                   
005900*                                 ANSKAFFARNUMMER                         
006000     03 IDINK-CDC            PIC X(4).                                    
006100*                                 INKÖPARNUMMER                           
006200     03 KDSORT               PIC X(2).                                    
006300*                                 SORT-KOD                                
006400     03 PRMATRL-ATTR         PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 PRMATRL              PIC Z(6)9.9(2).                              
006700*                                 FAST PRIS UNDER LÖPANDE ÅR              
006800     03 KVPB-REF-ATTR        PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 KVPB-REF             PIC Z(6)9.9.                                 
007100*                                 PERIODBEHOV REFILLING                   
007200     03 TIREFMPB-ATTR        PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 TIREFMPB             PIC 9(6).                                    
007500*                                 DATUM MANUELL PROGNOS REFILLING         
007600     03 DAPUBL-ATTR          PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 DAPUBL               PIC 9(5).                                    
007900*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
008000     03 TILEVBEG-ATTR        PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 TILEVBEG             PIC 9(4).                                    
008300*                                 ÅR - VECKA  (ÅÅVV)                      
008400     03 KVPROG-ATTR          PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600     03 KVPROG               PIC Z(6)9.                                   
008700*                                 ÅRSPROGNOS                              
008800     03 IDINK-ATTR           PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 IDINK                PIC Z(2)9.                                   
009100*                                 INKÖPARNUMMER                           
009200     03 IDANSK-ATTR          PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 IDANSK               PIC Z(2)9.                                   
009500*                                 ANSKAFFARNUMMER                         
009600     03 TEMFSINF             PIC X(55).                                   
009700*                                 INFORMATIONSMEDDELANDE                  
009800*** END OF VILMAII-COPY LENGTH= 445 BYTES                                 
