000100 01  MOD-W2O40601.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W2O406                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR.                                                      
001100        05 MOD-IDARTNR-UT    PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300        05 MOD-DASH          PIC X.                                       
001400        05 MOD-REKSIFFR      PIC X.                                       
001500*                                 KONTROLLSIFFRA                          
001600     03 MOD-IDDC-IN          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MOD-IDDC-UT          PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 MOD-IDLEVNR-IN       PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200     03 MOD-IDLEVNR-UT       PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400     03 MOD-IDLEVNR-SHIP-UT  PIC X(5).                                    
002500*                                 SKEPPANDE LEVERANTÖR                    
002600     03 MOD-UPDATE-LINE      OCCURS 8 TIMES.                              
002700        05 MOD-KDCMD-UPD-ATTR                                             
002800                             PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000        05 MOD-KDCMD-UPD     PIC X.                                       
003100*                                 RAD-UPPDATERINGSKOMMANDO                
003200*                                  BLANK  = INGENTING                     
003300*                                  D , B  = DELETE                        
003400*                                  R , Ä  = REPLACE                       
003500*                                  I,N,A  = INSERT                        
003600*                                  S , V  = SELECT                        
003700*                                  P , P  = PRINT                         
003800*                                  C , K  = COPY                          
003900        05 MOD-DALEVBSK-AVS  PIC Z(5).                                    
004000*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
004100        05 MOD-DALEVBSK-AVS-UPD-ATTR                                      
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-DALEVBSK-AVS-UPD                                           
004500                             PIC 9(5).                                    
004600*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
004700        05 MOD-KVAVIS        PIC Z(6)9.                                   
004800*                                 AVISERAT ANTAL                          
004900        05 MOD-KVAVIS-UPD-ATTR                                            
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-KVAVIS-UPD    PIC Z(6)9.                                   
005300*                                 AVISERAT ANTAL                          
005400        05 MOD-TILEVBSK-INL  PIC Z(5).                                    
005500*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
005600        05 MOD-TILEVBSK-INL-UPD-ATTR                                      
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-TILEVBSK-INL-UPD                                           
006000                             PIC 9(5).                                    
006100*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
006200        05 MOD-TILEVBSK-DISP PIC Z(5).                                    
006300*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
006400        05 MOD-FLFORAVI      PIC X.                                       
006500*                                 FÖRAVISERAD INLEVERANS                  
006600        05 MOD-IDLEVNR       PIC X(5).                                    
006700*                                 LEVERANTÖRNUMMER                        
006800     03 MOD-INSERT-LINE.                                                  
006900        05 MOD-DALEVBSK-AVS-NY-ATTR                                       
007000                             PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200        05 MOD-DALEVBSK-AVS-NY                                            
007300                             PIC X(5).                                    
007400*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
007500        05 MOD-KVAVIS-NY-ATTR                                             
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-KVAVIS-NY     PIC Z(6)9.                                   
007900*                                 AVISERAT ANTAL                          
008000        05 MOD-TILEVBSK-INL-NY-ATTR                                       
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-TILEVBSK-INL-NY                                            
008400                             PIC X(5).                                    
008500*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
008600        05 MOD-IDLEVNR-NY-ATTR                                            
008700                             PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900        05 MOD-IDLEVNR-NY    PIC X(5).                                    
009000*                                 LEVERANTÖRNUMMER                        
009100        05 MOD-IDLEVNR-SHIP-NY-ATTR                                       
009200                             PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400        05 MOD-IDLEVNR-SHIP-NY                                            
009500                             PIC X(5).                                    
009600*                                 SKEPPANDE LEVERANTÖR                    
009700     03 MOD-TEXT.                                                         
009800        05 MOD-TELEVBSK-TEXT1-ATTR                                        
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100        05 MOD-TELEVBSK-TEXT1                                             
010200                             PIC X(80).                                   
010300*                                 LEVERANSBESKEDSINFORMATION              
010400        05 MOD-TELEVBSK-TEXT2-ATTR                                        
010500                             PIC X(2).                                    
010600*                                 MFS ATTRIBUTFÄLT                        
010700        05 MOD-TELEVBSK-TEXT2                                             
010800                             PIC X(80).                                   
010900*                                 LEVERANSBESKEDSINFORMATION              
011000        05 MOD-TELEVBSK-TEXT3-ATTR                                        
011100                             PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300        05 MOD-TELEVBSK-TEXT3                                             
011400                             PIC X(80).                                   
011500*                                 LEVERANSBESKEDSINFORMATION              
011600        05 MOD-TELEVBSK-TEXT4-ATTR                                        
011700                             PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900        05 MOD-TELEVBSK-TEXT4                                             
012000                             PIC X(80).                                   
012100*                                 LEVERANSBESKEDSINFORMATION              
012200        05 MOD-TIBORT-ATTR   PIC X(2).                                    
012300*                                 MFS ATTRIBUTFÄLT                        
012400        05 MOD-TIBORT        PIC 9(5).                                    
012500*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
012600     03 MOD-TEMFSINF         PIC X(55).                                   
012700*                                 INFORMATIONSMEDDELANDE                  
012800*** END OF VILMAII-COPY LENGTH= 942 BYTES                                 
