000100 01  AREA.                                                                
000200*                                 LÄNKAREA NR 1                           
000300*                                 ANVÄNDS VID LAES-ARTIKELBAS             
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
001100*                                 LAGEROMRÅDE                             
001200*                                 AREA                                    
001300     03 ADGANG               PIC S9(3)           COMP-3.                  
001400*                                 GÅNG                                    
001500*                                 AISLE                                   
001600     03 ADPLATS              PIC S9(5)           COMP-3.                  
001700*                                 LAGERPLATSNUMMER                        
001800*                                 LOCATION                                
001900     03 KVAKS                PIC S9(7)           COMP-3.                  
002000*                                 ANKOMSTSALDO                            
002100*                                 ADVICED,NOT BINNED,QTY                  
002200     03 KVLS                 PIC S9(7)           COMP-3.                  
002300*                                 LAGERSALDO                              
002400*                                 STOCK BALANCE                           
002500     03 KVEFRS               PIC S9(7)           COMP-3.                  
002600*                                 EJ FAKTURERAT ANTAL STYCK               
002700*                                 ORDERED NOT INVOICED QTY                
002800     03 KVPB-SEP             PIC S9(6)V9(1)      COMP-3.                  
002900*                                 SEPARAT PERIODBEHOV                     
003000*                                 SEPARATE PERIOD REQUIREMENTS            
