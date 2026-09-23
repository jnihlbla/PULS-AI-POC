000100 01  MOD-W2O10401.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-KVPB-CDC-IN-ATTR PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-KVPB-CDC-IN      PIC X(2).                                    
001400*                                 MFS BEHANDLING AV INPUTFÄLT             
001500     03 MOD-KVPB-C2-IN-ATTR  PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-KVPB-C2-IN       PIC X(2).                                    
001800*                                 MFS BEHANDLING AV INPUTFÄLT             
001900     03 MOD-FLMPB-ATTR       PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-FLMPB            PIC X(3).                                    
002200     03 MOD-FLMPB-IN-ATTR    PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-FLMPB-IN         PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-BEART            PIC X(25).                                   
002700*                                 ARTIKELBENÄMNING                        
002800     03 MOD-VECKA-I-AKT-PER  PIC X(6).                                    
002900     03 MOD-FILLER           OCCURS 12 TIMES.                             
003000        05 MOD-TIAARP        PIC X(4).                                    
003100*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
003200*                                 12 PER ÅR                               
003300     03 MOD-FILLER           OCCURS 12 TIMES.                             
003400        05 MOD-KVVIPER       PIC 9.                                       
003500*                                 ANT VECKOR I REDOVISNINGSPERIOD         
003600     03 MOD-FILLER.                                                       
003700        05 MOD-KVOI-CDC      OCCURS 12 TIMES                              
003800                             PIC Z(5)9.                                   
003900*                                 ORDERINGÅNG I STYCK PER TIDSENH         
004000        05 MOD-KVOI-DCVK     OCCURS 12 TIMES                              
004100                             PIC Z(5)9.                                   
004200*                                 ORDERINGÅNG I STYCK PER TIDSENH         
004300        05 MOD-KVOI-SATS     OCCURS 12 TIMES                              
004400                             PIC Z(5)9.                                   
004500*                                 ORDERINGÅNG I STYCK PER TIDSENH         
004600        05 MOD-KVOI-DIV      OCCURS 12 TIMES                              
004700                             PIC Z(5)9.                                   
004800*                                 ORDERINGÅNG I STYCK PER TIDSENH         
004900     03 MOD-FILLER.                                                       
005000        05 MOD-KVOI-INNEV-CDC                                             
005100                             PIC Z(5)9.                                   
005200        05 MOD-KVOI-INNEV-DCVK                                            
005300                             PIC Z(5)9.                                   
005400        05 MOD-KVOI-INNEV-SATS                                            
005500                             PIC Z(5)9.                                   
005600        05 MOD-KVOI-INNEV-DIV                                             
005700                             PIC Z(5)9.                                   
005800     03 MOD-FILLER.                                                       
005900        05 MOD-KVOI-RULL-12-CDC                                           
006000                             PIC Z(7).                                    
006100        05 MOD-KVOI-RULL-12-DCVK                                          
006200                             PIC Z(7).                                    
006300        05 MOD-KVOI-RULL-12-SATS                                          
006400                             PIC Z(7).                                    
006500        05 MOD-KVOI-RULL-12-DIV                                           
006600                             PIC Z(7).                                    
006700     03 MOD-FILLER           OCCURS 3 TIMES.                              
006800        05 MOD-AAR           PIC 9(2).                                    
006900*                                 ÅR    (ÅÅ)                              
007000        05 MOD-AAR-CDC       PIC Z(7).                                    
007100        05 MOD-AAR-DCVK      PIC Z(7).                                    
007200        05 MOD-AAR-SATS      PIC Z(7).                                    
007300        05 MOD-AAR-DIV       PIC Z(7).                                    
007400     03 MOD-FILLER.                                                       
007500        05 MOD-AARSFORB-CDC  PIC Z(7).                                    
007600        05 MOD-AARSFORB-DC   PIC Z(7).                                    
007700     03 MOD-FILLER.                                                       
007800        05 MOD-AARSFORB-HALV-CDC                                          
007900                             PIC Z(7).                                    
008000        05 MOD-AARSFORB-HALV-DC                                           
008100                             PIC Z(7).                                    
008200     03 MOD-FILLER.                                                       
008300        05 MOD-KVPB-SEP-CDC-ATTR                                          
008400                             PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-KVPB-SEP-CDC  PIC Z(5)9.9.                                 
008700*                                 SEPARAT PERIODBEHOV                     
008800        05 MOD-KVPB-SEP-DC-ATTR                                           
008900                             PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100        05 MOD-KVPB-SEP-DC   PIC Z(5)9.9.                                 
009200*                                 SEPARAT PERIODBEHOV                     
009300     03 MOD-FLOREGPB-ATTR    PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-FLOREGPB         PIC X(3).                                    
009600     03 MOD-FLOREGPB-IN-ATTR PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800     03 MOD-FLOREGPB-IN      PIC X(2).                                    
009900*                                 MFS BEHANDLING AV INPUTFÄLT             
010000     03 MOD-KVPB-PLAN        PIC Z(5)9.9.                                 
010100*                                 PLANERAT PERIODBEHOV                    
010200     03 MOD-DAPBPLAN         PIC X(6).                                    
010300     03 MOD-PBPLANTYP        PIC X(4).                                    
010400     03 MOD-KVPB-SEP-CDC-2   PIC Z(5)9.9.                                 
010500*                                 SEPARAT PERIODBEHOV                     
010600     03 MOD-KVPB-SEP-DC-2    PIC Z(5)9.9.                                 
010700*                                 SEPARAT PERIODBEHOV                     
010800     03 MOD-KVPB-SATS        PIC Z(5)9.9.                                 
010900*                                 SATS-PERIODBEHOV                        
011000     03 MOD-TIPBLOCK         PIC 9(6).                                    
011100*                                 FRAMTIDA DATUM FÖR PB-LÅSNING Å         
011200*                                 ÅMMDD                                   
011300     03 MOD-TIPBLOCK-IN-ATTR PIC X(2).                                    
011400*                                 MFS ATTRIBUTFÄLT                        
011500     03 MOD-TIPBLOCK-IN      PIC X(6).                                    
011600*                                 FRAMTIDA DATUM FÖR PB-LÅSNING Å         
011700*                                 ÅMMDD                                   
011800     03 MOD-KVPB-TREND       PIC +(6)9.9.                                 
011900*                                 PERIODTRENDVÄRDE                        
012000     03 MOD-TEMFSINF         PIC X(55).                                   
012100*                                 INFORMATIONSMEDDELANDE                  
012200*** END OF VILMAII-COPY LENGTH= 770 BYTES                                 
