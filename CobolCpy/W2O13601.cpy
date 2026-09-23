000100 01  MOD-W2O13601.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-BEART            PIC X(25).                                   
001200*                                 ARTIKELBENÄMNING                        
001300     03 MOD-FILLER           OCCURS 24 TIMES.                             
001400        05 MOD-TIPER         PIC 9(3).                                    
001500     03 MOD-FILLER.                                                       
001600        05 MOD-KVOI-CDC      OCCURS 24 TIMES                              
001700                             PIC Z(5)9.                                   
001800*                                 ORDERINGÅNG I STYCK PER TIDSENH         
001900        05 MOD-KVOI-DC-REF   OCCURS 24 TIMES                              
002000                             PIC Z(5)9.                                   
002100*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002200        05 MOD-KVOI-DC-KUND  OCCURS 24 TIMES                              
002300                             PIC Z(5)9.                                   
002400*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002500     03 MOD-FILLER.                                                       
002600        05 MOD-KVOI-INNEV-CDC                                             
002700                             PIC Z(5)9.                                   
002800*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002900        05 MOD-KVOI-INNEV-DC-REF                                          
003000                             PIC Z(5)9.                                   
003100*                                 ORDERINGÅNG I STYCK PER TIDSENH         
003200        05 MOD-KVOI-INNEV-DC-KUND                                         
003300                             PIC Z(5)9.                                   
003400*                                 ORDERINGÅNG I STYCK PER TIDSENH         
003500     03 MOD-FILLER.                                                       
003600        05 MOD-KVOI-RULL-12-CDC                                           
003700                             PIC Z(6)9.                                   
003800        05 MOD-KVOI-RULL-12-DC-REF                                        
003900                             PIC Z(6)9.                                   
004000        05 MOD-KVOI-RULL-12-DC-KUND                                       
004100                             PIC Z(6)9.                                   
004200     03 MOD-FILLER           OCCURS 3 TIMES.                              
004300        05 MOD-AAR           PIC 9(2).                                    
004400*                                 ÅR    (ÅÅ)                              
004500        05 MOD-AAR-CDC       PIC Z(6)9.                                   
004600        05 MOD-AAR-DC-REF    PIC Z(6)9.                                   
004700        05 MOD-AAR-DC-KUND   PIC Z(6)9.                                   
004800     03 MOD-KVPB-MASK        PIC Z(5)9.9.                                 
004900*                                 PERIODBEHOV (PROGNOS)                   
005000     03 MOD-KVPB-PLAN-ATTR   PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-KVPB-PLAN-IN     PIC X(8).                                    
005300*                                 PLANERAT PERIODBEHOV                    
005400     03 MOD-KVPB-PLAN-UT     PIC X(8).                                    
005500*                                 PLANERAT PERIODBEHOV                    
005600     03 MOD-TIPBPLAN-ATTR    PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-TIPBPLAN-IN      PIC X(6).                                    
005900*                                 DATUM KVPB-PLAN GILTIG I EN PER         
006000*                                 IOD                                     
006100     03 MOD-TIPBPLAN-UT      PIC X(6).                                    
006200*                                 DATUM KVPB-PLAN GILTIG I EN PER         
006300*                                 IOD                                     
006400     03 MOD-KVPB-PLAN-JUST1-IN-ATTR                                       
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 MOD-KVPB-PLAN-JUST1-IN                                            
006800                             PIC X(8).                                    
006900*                                 PLANERAT PERIODBEHOV JUST1              
007000     03 MOD-KVPB-PLAN-JUST1-UT                                            
007100                             PIC X(8).                                    
007200*                                 PLANERAT PERIODBEHOV JUST1              
007300     03 MOD-TIPBPLAN-JUST1-FOM-IN-ATTR                                    
007400                             PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600     03 MOD-TIPBPLAN-JUST1-FOM-IN                                         
007700                             PIC X(4).                                    
007800*                                 FOM PB-PLAN DATUM - JUST1               
007900     03 MOD-TIPBPLAN-JUST1-FOM-UT                                         
008000                             PIC X(4).                                    
008100*                                 FOM PB-PLAN DATUM - JUST1               
008200     03 MOD-TIPBPLAN-JUST1-TOM-IN-ATTR                                    
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 MOD-TIPBPLAN-JUST1-TOM-IN                                         
008600                             PIC X(6).                                    
008700*                                 TOM PB-PLAN DATUM - JUST1               
008800     03 MOD-TIPBPLAN-JUST1-TOM-UT                                         
008900                             PIC 9(6).                                    
009000*                                 TOM PB-PLAN DATUM - JUST1               
009100     03 MOD-KVPB-PLAN-JUST2-IN-ATTR                                       
009200                             PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-KVPB-PLAN-JUST2-IN                                            
009500                             PIC X(8).                                    
009600*                                 PLANERAT PERIODBEHOV JUST2              
009700     03 MOD-KVPB-PLAN-JUST2-UT                                            
009800                             PIC X(8).                                    
009900*                                 PLANERAT PERIODBEHOV JUST2              
010000     03 MOD-TIPBPLAN-JUST2-FOM-IN-ATTR                                    
010100                             PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300     03 MOD-TIPBPLAN-JUST2-FOM-IN                                         
010400                             PIC X(4).                                    
010500*                                 FOM PB-PLAN DATUM - JUST2               
010600     03 MOD-TIPBPLAN-JUST2-FOM-UT                                         
010700                             PIC X(4).                                    
010800*                                 FOM PB-PLAN DATUM - JUST2               
010900     03 MOD-TIPBPLAN-JUST2-TOM-IN-ATTR                                    
011000                             PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 MOD-TIPBPLAN-JUST2-TOM-IN                                         
011300                             PIC X(6).                                    
011400*                                 TOM PB-PLAN DATUM - JUST2               
011500     03 MOD-TIPBPLAN-JUST2-TOM-UT                                         
011600                             PIC 9(6).                                    
011700*                                 TOM PB-PLAN DATUM - JUST2               
011800     03 MOD-KVPB-CDC         PIC Z(7).Z.                                  
011900*                                 PERIODBEHOV (PROGNOS)                   
012000     03 MOD-KVPB-DC          PIC Z(7).Z.                                  
012100*                                 PERIODBEHOV (PROGNOS)                   
012200     03 MOD-KVOI-SNITT-12-CDC                                             
012300                             PIC Z(5)9.                                   
012400*                                 ORDERINGÅNG I STYCK PER TIDSENH         
012500     03 MOD-KVOI-SNITT-12-DC-REF                                          
012600                             PIC Z(5)9.                                   
012700*                                 ORDERINGÅNG I STYCK PER TIDSENH         
012800     03 MOD-TEMFSINF         PIC X(55).                                   
012900*                                 INFORMATIONSMEDDELANDE                  
013000*** END OF VILMAII-COPY LENGTH= 908 BYTES                                 
