000100 01  W33018.                                                              
000200*                                 SALES-AND-TARGET                        
000300     03 IDARTNR              PIC 9(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 TIFSGVV              PIC 9(6).                                    
000600*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
000700     03 IDDISTR              PIC X(5).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 SULEVANT             PIC 9(9).                                    
001000*                                 SUMMA LEVERERAT ANTAL                   
001100*                                 AV 1 ARTIKEL                            
001200     03 SUARTFSG             PIC 9(9)V9(2).                               
001300*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
001400*                                                                         
001500     03 SULEVANT-DO          PIC 9(9).                                    
001600*                                 ANTAL LEVERERADE ARTIKLAR               
001700*                                 PÅ DAGORDER                             
001800     03 SUARTFSG-DO          PIC 9(9)V9(2).                               
001900*                                 SUMMA FSG/ARTIKEL PÅ                    
002000*                                 DAGORDER                                
002100     03 SULEVANT-KRE         PIC 9(9).                                    
002200*                                 ANTAL LEV. ART. SOM                     
002300*                                 KREDITERATS                             
002400     03 SUARTFSG-KRE         PIC 9(9)V9(2).                               
002500*                                 SUMMA KREDITERAD FSG/                   
002600*                                 ARTIKEL                                 
002700     03 PRARTNTO-BULK        PIC 9(7)V9(2).                               
002800*                                 ARTIKELPRIS NETTO                       
002900     03 PRARTNTO-DO          PIC 9(7)V9(2).                               
003000*                                 ARTIKELPRIS NETTO                       
003100     03 DADATUM              PIC 9(8).                                    
003200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
003300*** END OF VILMAII-COPY LENGTH= 106 BYTES                                 
