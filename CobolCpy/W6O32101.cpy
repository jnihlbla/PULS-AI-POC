000100 01  MOD-W6O32101.                                                        
000200*                                 COPYTEXT FÖR MOD W6I32101               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN-ATTR  PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDDC-IN-ATTR     PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-IDDC-IN          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDARTNR-UT-ATTR  PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-IDDC-UT-ATTR     PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-BEART            PIC X(25).                                   
002400*                                 ARTIKELBENÄMNING                        
002500     03 MOD-IDDC-SEND-TO-ATTR                                             
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-IDDC-SEND-TO     PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000     03 MOD-KDERS            PIC Z9.                                      
003100*                                 ERSÄTTNINGSKOD                          
003200     03 MOD-IDLEVNR          PIC X(5).                                    
003300*                                 LEVERANTÖRNUMMER                        
003400     03 MOD-IDANSK           PIC Z(2)9.                                   
003500*                                 ANSKAFFARNUMMER                         
003600     03 MOD-KVSTOCK          PIC -(6)9.                                   
003700*                                 LAGERSALDO                              
003800     03 MOD-KVAKS-SDC        PIC Z(6)9.                                   
003900*                                 ANKOMSTSALDO                            
004000     03 MOD-KVAKS-PAV        PIC Z(6)9.                                   
004100*                                 ANKOMSTSALDO                            
004200     03 MOD-KVBEART          PIC -(6)9.                                   
004300*                                 BESTÄLLT ANTAL STYCKEN                  
004400     03 MOD-KDLEVSP          PIC Z9.                                      
004500*                                 SPÄRRKOD LEVERANS                       
004600     03 MOD-KVSPARR-KVAL     PIC Z(6)9.                                   
004700*                                 SPÄRRAT ANTAL KVALITETSFEL              
004800     03 MOD-TIRETUR-BEORD    PIC 9(6).                                    
004900*                                 RETURDATUM                              
005000     03 MOD-KVRETUR-BEORD-ATTR                                            
005100                             PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-KVRETUR-BEORD    PIC Z(7).                                    
005400*                                 ANTAL SENASTE RETURORDER                
005500     03 MOD-KDORDKL-ATTR     PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-KDORDKL          PIC X.                                       
005800*                                 ORDERKLASS                              
005900     03 MOD-RETUR-TEXT-ATTR  PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-RETUR-TEXT       PIC X(60).                                   
006200     03 MOD-TISKROT          PIC 9(6).                                    
006300*                                 SKROTNINGSDATUM                         
006400     03 MOD-KVSKROT-ATTR     PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 MOD-KVSKROT          PIC Z(7).                                    
006700*                                 ANTAL SENASTE SKROTORDER                
006800     03 MOD-VALUE-OF-SCRAP-QTY                                            
006900                             PIC Z(7)9.9(2).                              
007000     03 MOD-SKROT-TEXT-ATTR  PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-SKROT-TEXT       PIC X(60).                                   
007300     03 MOD-IDPERSON-QUAL-ATTR                                            
007400                             PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600     03 MOD-IDPERSON-QUAL    PIC Z(3).                                    
007700*                                 PERSONKOD                               
007800     03 MOD-IDPERSON-ESC-ATTR                                             
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100     03 MOD-IDPERSON-ESC     PIC Z(3).                                    
008200*                                 PERSONKOD                               
008300     03 MOD-IDKONTO-ATTR     PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 MOD-IDKONTO          PIC Z(10).                                   
008600*                                 KONTO                                   
008700     03 MOD-IDANALYS-ATTR    PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 MOD-IDANALYS         PIC X(12).                                   
009000*                                 ANALYSNUMMER                            
009100     03 MOD-FLAG-ABUFFER-ATTR                                             
009200                             PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-FLAG-ABUFFER     PIC X.                                       
009500*                                 ALLMÄN FLAGGA                           
009600     03 MOD-TEMFSINF         PIC X(55).                                   
009700*                                 INFORMATIONSMEDDELANDE                  
009800*** END OF VILMAII-COPY LENGTH= 412 BYTES                                 
