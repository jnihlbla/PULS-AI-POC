000100 01  W1152001.                                                            
000200*                                 LADDNING-POST   TILL  RDE3              
000300*                                                                         
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 KDCLAGER             PIC S9              COMP-3.                  
000900*                                 CENTRALLAGERKOD                         
001000     03 TIBEHOV              PIC S9(5)           COMP-3.                  
001100*                                 BEHOVSVECKA           (≈≈VV)            
001200     03 SUTPO-EJPB           PIC S9(7)           COMP-3.                  
001300*                                 TPO-KVANTITET, EJ BEHOVSP≈VERKA         
001400*                                 NDE                                     
001500     03 TIBASORD             PIC S9(7)           COMP-3.                  
001600*                                 ORDERGENERERING FR≈N BASLAGER           
001700*** END COPY W1152001    LENGTH=20                                        
