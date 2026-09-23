000100 01  MOD-W2O14301.                                                        
000200*                                 MODCOPYTEXT TILL W20143.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC Z(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-KVEOQ            PIC Z(6)9.                                   
001200*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
001300*                                 ET                                      
001400     03 MOD-KDPRODSL         PIC Z9.                                      
001500*                                 PRODUKTSLAG                             
001600     03 MOD-KVSLAGER         PIC Z(6)9.                                   
001700*                                 SÄKERHETSLAGER                          
001800     03 MOD-KDPRISKL         PIC X.                                       
001900*                                 PRISKLASS                               
002000     03 MOD-KVSLAGER-OPT     PIC Z(6)9.                                   
002100*                                 OPTIMALT SÄKERHETSLAGER                 
002200     03 MOD-KDFREKKL         PIC X.                                       
002300*                                 FREKVENSKLASS                           
002400     03 MOD-RETARGET         PIC 9(2).9.                                  
002500*                                                                         
002600     03 MOD-IDLEVNR          PIC X(5).                                    
002700*                                 LEVERANTÖRNUMMER                        
002800     03 MOD-RESLJUST         PIC 9(2).9.                                  
002900*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
003000     03 MOD-BEFT             PIC Z(2)9.                                   
003100*                                 FÖRPACKNINGSTYP                         
003200     03 MOD-KVVECKOR-MINSL   PIC 9(2).9.                                  
003300*                                 MINGRÄNS SÄKERHETSLAGER                 
003400     03 MOD-ADLAGOMR         PIC Z(2)9.                                   
003500*                                 LAGEROMRÅDE                             
003600     03 MOD-KVVECKOR-MAXSL   PIC 9(2).9.                                  
003700*                                 MAXGRÄNS SÄKERHETSLAGER                 
003800     03 MOD-PRORDSK          PIC Z(4)9.9(2).                              
003900*                                 ORDERSÄRKOSTNAD                         
004000     03 MOD-KVVECKOR-LVAR-LEV                                             
004100                             PIC 9(2).9.                                  
004200*                                 VARIANS I LEDTIDEN                      
004300     03 MOD-REOKOST-LEV      PIC 9(2).9(2).                               
004400*                                 FAKTOR ORDERKOSTNAD/LEV                 
004500     03 MOD-KVVECKOR-LVAR-ART                                             
004600                             PIC 9(2).9.                                  
004700*                                 VARIANS I LEDTIDEN                      
004800     03 MOD-REOKOST-BEFT     PIC 9(2).9(2).                               
004900*                                 FAKTOR ORDERKOSTNAD/BEFT                
005000     03 MOD-REOKOST-OMR      PIC 9(2).9(2).                               
005100*                                 FAKTOR ORDERKOSTNAD/ADLAGOMR            
005200     03 MOD-REVKOST-OMR      PIC 9(2).9(2).                               
005300*                                 FAKTOR VOLYMKOSTNAD/ADLAGOMR            
005400     03 MOD-REOLAGK          PIC 9(2).                                    
005500*                                 PROCENT ÖVERLAGERKOSTNAD                
005600     03 MOD-PRLAGK           PIC Z(4)9.9(2).                              
005700*                                 LAGRINGSKOSTNAD / M3                    
005800     03 MOD-RELAGR           PIC 9(2).                                    
005900*                                 PROCENT LAGERHÅLLNINGSKOSTNAD           
006000     03 MOD-KVQ              PIC Z(6)9.                                   
006100*                                 EKONOMISK HEMTAGNINGSKVANTITET          
006200     03 MOD-KVULOAD          PIC Z(6)9.                                   
006300*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
006400     03 MOD-KVPALL           PIC Z(6)9.                                   
006500*                                 ANTAL I PALL                            
006600     03 MOD-KVMAD-TOT        PIC Z(5)9.9.                                 
006700*                                 TOTALT PROGNOSFEL                       
006800     03 MOD-KVPB-PLAN        PIC Z(5)9.9.                                 
006900*                                 PLANERAT PERIODBEHOV                    
007000     03 MOD-TEMFSINF         PIC X(55).                                   
007100*                                 INFORMATIONSMEDDELANDE                  
007200*** END OF VILMAII-COPY LENGTH= 254 BYTES                                 
