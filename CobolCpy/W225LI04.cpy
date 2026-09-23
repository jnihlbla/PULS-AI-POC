000100 01  W225LI04.                                                            
000200*                                 LISTRECORD FÖR SERVICE GRAD PER         
000300*                                 LV/PV INDEX1 = 1:A VECKAN INOM          
000400*                                 PERIODEN INDEX2 = INNEVARANDE           
000500*                                 VECKA INDEX3 = SUMMAN AV ALLA           
000600*                                 VECKOR INOM PERIODEN                    
000700*                                                                         
000800     03 GEMENSAMT.                                                        
000900        05 IDARTNR           PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100        05 IDLKTO            PIC S9(7)           COMP-3.                  
001200*                                 LAGERKONTO (FFHHHUU)                    
001300        05 KDCLPOST          PIC S9              COMP-3.                  
001400*                                 CENTRALLAGERPOST                        
001500        05 SUROBEL-CDC       PIC S9(7)V9(2)      COMP-3.                  
001600*                                 RESTORDERVÄRDE STANDARDPRIS             
001700        05 SUROBEL-SDC       PIC S9(7)V9(2)      COMP-3.                  
001800*                                 RESTORDERVÄRDE STANDARDPRIS             
001900        05 IDPROJ            PIC X(4).                                    
002000*                                 PARTS PROJEKTIDENTITET                  
002100     03 CDC-INFO             OCCURS 3 TIMES.                              
002200        05 KVRORAD-CDC-1-2   PIC S9(7)V9(2)      COMP-3.                  
002300*                                 RESTNOTERADE RADER                      
002400*                                 ORDERKLASS 1 OCH 2  (KVRORAD-00         
002500*                                 3)                                      
002600        05 KVRORAD-CDC-3-4   PIC S9(7)V9(2)      COMP-3.                  
002700*                                 RESTNOTERADE RADER                      
002800*                                 ORDERKLASS 3 - 4  (KVRORAD-003)         
002900        05 KVAVBRAD-CDC      PIC S9(7)V9(2)      COMP-3.                  
003000*                                 AVBOKADE RADER                          
003100        05 KVFYSAVV-CDC      PIC S9(7)V9(2)      COMP-3.                  
003200*                                                    KVFYSAVV-002         
003300*                                 FYSISK AVVIKELSE RADER                  
003400        05 KVINORD-CDC       PIC S9(7)           COMP-3.                  
003500*                                 ANTAL INKOMNA ORDERRADER                
003600     03 SDC-INFO             OCCURS 3 TIMES.                              
003700        05 KVRORAD-SDC-1-2   PIC S9(7)V9(2)      COMP-3.                  
003800*                                 RESTNOTERADE RADER                      
003900*                                 ORDERKLASS 1 OCH 2  (KVRORAD-00         
004000*                                 3)                                      
004100        05 KVRORAD-SDC-3-4   PIC S9(7)V9(2)      COMP-3.                  
004200*                                 RESTNOTERADE RADER                      
004300*                                 ORDERKLASS 3 - 4  (KVRORAD-003)         
004400        05 KVAVBRAD-SDC      PIC S9(7)V9(2)      COMP-3.                  
004500*                                 AVBOKADE RADER                          
004600        05 KVFYSAVV-SDC      PIC S9(7)V9(2)      COMP-3.                  
004700*                                                    KVFYSAVV-002         
004800*                                 FYSISK AVVIKELSE RADER                  
004900        05 KVINORD-SDC       PIC S9(7)           COMP-3.                  
005000*                                 ANTAL INKOMNA ORDERRADER                
005100*** END OF VILMAII-COPY LENGTH= 168 BYTES                                 
