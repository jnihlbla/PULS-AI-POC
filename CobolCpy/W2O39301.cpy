000100 01  MOD-W2O39301.                                                        
000200*                                 MOD-COPYTEXT FOR W2O393                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-UTFALT.                                                       
001200*                                 UTDATA 2393                             
001300        05 MOD-IDLEVNR       PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500        05 MOD-KVSPANT       PIC Z(6)9.                                   
001600*                                 SPÄRRAT ANTAL                           
001700        05 MOD-IDPERSON-BUY  PIC Z(2)9.                                   
001800*                                 PERSONKOD REFILLANSVARIG                
001900        05 MOD-TIFINLV       PIC 9(5).                                    
002000*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
002100     03 MOD-INFALT.                                                       
002200*                                 INDATA 2393                             
002300        05 MOD-IDLEVNR-IN-ATTR                                            
002400                             PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600        05 MOD-IDLEVNR-IN    PIC X(5).                                    
002700*                                 LEVERANTÖRNUMMER                        
002800        05 MOD-KVSPANT-IN-ATTR                                            
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-KVSPANT-IN    PIC Z(6)9.                                   
003200*                                 SPÄRRAT ANTAL                           
003300        05 MOD-IDPERSON-BUY-IN-ATTR                                       
003400                             PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-IDPERSON-BUY-IN                                            
003700                             PIC Z(2)9.                                   
003800*                                 PERSONKOD REFILLANSVARIG                
003900        05 MOD-TIFINLV-IN-ATTR                                            
004000                             PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200        05 MOD-TIFINLV-IN    PIC 9(4).                                    
004300*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
004400     03 MOD-KVPB-PLAN-ATTR   PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-KVPB-PLAN-IN     PIC X(8).                                    
004700*                                 PLANERAT PERIODBEHOV                    
004800     03 MOD-KVPB-PLAN-UT     PIC X(8).                                    
004900*                                 PLANERAT PERIODBEHOV                    
005000     03 MOD-TIPBPLAN-ATTR    PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-TIPBPLAN-IN      PIC X(6).                                    
005300*                                 DATUM KVPB-PLAN GILTIG I EN PER         
005400*                                 IOD                                     
005500     03 MOD-TIPBPLAN-UT      PIC X(6).                                    
005600*                                 DATUM KVPB-PLAN GILTIG I EN PER         
005700*                                 IOD                                     
005800     03 MOD-KVPB-MASK        PIC Z(5)9.9.                                 
005900*                                 PERIODBEHOV (PROGNOS)                   
006000     03 MOD-TEARTNOT.                                                     
006100*                                 INDATA 2393                             
006200        05 MOD-TEARTNOT1-IN-ATTR                                          
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-TEARTNOT1-IN  PIC X(40).                                   
006600*                                 ARTIKEL NOTERING                        
006700        05 MOD-TEARTNOT2-IN-ATTR                                          
006800                             PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-TEARTNOT2-IN  PIC X(40).                                   
007100*                                 ARTIKEL NOTERING                        
007200     03 MOD-TEMFSINF         PIC X(55).                                   
007300*                                 INFORMATIONSMEDDELANDE                  
007400*** END OF VILMAII-COPY LENGTH= 288 BYTES                                 
