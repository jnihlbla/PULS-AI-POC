000100 01  W51334.                                                              
000200*                                 LISTPOST FÖR ART. MED FYSISK            
000300*                                 AVVIKELSE VID INVENTERING.              
000400*                                                                         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 KDSORT1              PIC S9              COMP-3.                  
001000*                                 SORTERINGSKOD                           
001100     03 KDINVKAT             PIC S9(3)           COMP-3.                  
001200*                                 INVENTERINGSKATEGORI                    
001300     03 ADARTADR             PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELADRESS                           
001500     03 TIJUSTDA             PIC S9(7)           COMP-3.                  
001600*                                 JUSTERINGSDATUM                         
001700     03 TIAVIDAT             PIC S9(7)           COMP-3.                  
001800*                                 AVISERINGSDATUM (YYMMDD)                
001900     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002000*                                 PRODUKTSLAG                             
002100     03 KVJUSTKV             PIC S9(7)           COMP-3.                  
002200*                                 JUSTERAD KVANTITET                      
002300     03 KVAVIS               PIC S9(7)           COMP-3.                  
002400*                                 AVISERAT ANTAL                          
002500     03 KVAKS                PIC S9(7)           COMP-3.                  
002600*                                 ANKOMSTSALDO                            
002700     03 KVLS                 PIC S9(7)           COMP-3.                  
002800*                                 LAGERSALDO                              
002900     03 KVEFRS               PIC S9(7)           COMP-3.                  
003000*                                 EJ FAKTURERAT ANTAL STYCK               
003100     03 KVUTRS               PIC S9(7)           COMP-3.                  
003200*                                 UTREDNINGSSALDO                         
003300     03 BEART                PIC X(25).                                   
003400*                                 ARTIKELBENÄMNING                        
003500*** END OF VILMAII-COPY LENGTH= 74 BYTES                                  
