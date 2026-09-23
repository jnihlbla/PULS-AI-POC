000100 01  W479023.                                                             
000200*                                                                         
000300     03 IDDISTR              PIC S9(5)           COMP-3.                  
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000600*                                 KUNDNUMMER                              
000700     03 IDPRODNR             PIC S9(7)           COMP-3.                  
000800*                                 PRODUKTIONSNUMMER                       
000900     03 IDORDER              PIC S9(7)           COMP-3.                  
001000*                                 VOLVO PARTS ORDERNUMMER                 
001100     03 KDCLAGER             PIC S9              COMP-3.                  
001200      88 KDCLAGER-C1         VALUE +1.                                    
001300      88 KDCLAGER-C2         VALUE +2.                                    
001400*                                 CENTRALLAGERKOD                         
001500     03 TIBEGPAC-C1          PIC S9(7)           COMP-3.                  
001600*                                 BEGÄRD PACKNINGSDAG C1 (ÅÅMMDD)         
001700     03 TIBEGPAC-C2          PIC S9(7)           COMP-3.                  
001800*                                 BEGÄRD PACKNINGSDAG C2 (ÅÅMMDD)         
001900     03 TIORDREG             PIC S9(7)           COMP-3.                  
002000*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002100     03 IDPLKLST             PIC S9(3)           COMP-3.                  
002200*                                 PLOCKLISTNUMMER                         
002300     03 IDARTNR              PIC S9(9)           COMP-3.                  
002400*                                 ARTIKELNUMMER                           
002500     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
002600*                                 LAGEROMRÅDE                             
002700     03 KDFRAKT              PIC S9(3)           COMP-3.                  
002800*                                 FRAKTSÄTT C1-C2 TILL KUND               
002900     03 KDORDKL              PIC S9              COMP-3.                  
003000      88 KDORDKL-VOR         VALUE +0.                                    
003100      88 KDORDKL-DAG         VALUE +1.                                    
003200      88 KDORDKL-2           VALUE +2.                                    
003300      88 KDORDKL-SNABB       VALUE +2.                                    
003400      88 KDORDKL-SPECIAL     VALUE +3.                                    
003500      88 KDORDKL-KVANT       VALUE +4.                                    
003600      88 KDORDKL-SATS        VALUE +5.                                    
003700*                                 ORDERKLASS                              
003800     03 KVBEART              PIC S9(7)           COMP-3.                  
003900*                                 BESTÄLLT ANTAL ARTIKLAR                 
004000     03 KVLEVART             PIC S9(7)           COMP-3.                  
004100*                                 LEVERERAT ANTAL ARTIKLAR                
004200     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
004300*                                 ARTIKELVIKT NETTO (KG)                  
004400     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
004500*                                 ARTIKELVOLYM NETTO (CM3)                
004600     03 FLDIRLEV             PIC X.                                       
004700*                                 DIREKTLEVERANS ?                        
004800     03 IDKUNDRF-RO          PIC X(10).                                   
004900*                                 KUND REF PÅ RO                          
005000     03 IDKOLLI              PIC S9(5)           COMP-3.                  
005100*                                 KOLLINUMMER                             
005200     03 IDUSER               PIC X(8).                                    
005300*                                 ANVÄNDARENS SÄKERHETS ID                
005400     03 KVLEVART2            PIC S9(7)           COMP-3.                  
005500*                                 FAKTISKT LEVERERAT ANTAL                
005600     03 IDPRC.                                                            
005700*                                 PRODUKTIONSKANAL                        
005800        05 IDPRCBAS          PIC X(3).                                    
005900*                                 PRC-BAS                                 
006000        05 IDPRCVAR          PIC X.                                       
006100*                                 PRC-VARIANT                             
006200     03 IDSHIFT              PIC X.                                       
006300*                                 SHIFT IDENTITET                         
006400     03 KDPRODSL             PIC S9(3)           COMP-3.                  
006500*                                 PRODUKTSLAG                             
006600     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
006700*                                 ARTIKELSTANDARDPRIS                     
006800     03 TIPACKN              PIC S9(7)           COMP-3.                  
006900*                                 PACKNINGSDATUM         (ÅÅMMDD)         
007000*** END COPY W479023     LENGTH=99                                        
