000100 01  W231L002.                                                            
000200*                                 LÄNKAREA FÖR LÄSNING AV                 
000300*                                 INV.SEGM                                
000400     03 KDCALL               PIC S9(3)           COMP-3.                  
000500      88 LAES-INVENTERING    VALUE +2.                                    
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 FLJANEJ-INVSEG       PIC X.                                       
000900*                                 JA/NEJ-FLAGGA                           
001000     03 KDCLAGER             PIC S9              COMP-3.                  
001100      88 KDCLAGER-C1         VALUE +1.                                    
001200      88 KDCLAGER-C2         VALUE +2.                                    
001300*                                 CENTRALLAGERKOD                         
001400     03 IO-AREA.                                                          
001500        05 TIJUSTDA          PIC S9(7)           COMP-3.                  
001600*                                 JUSTERINGSDATUM                         
001700        05 KVJUSTKV          PIC S9(7)           COMP-3.                  
001800*                                 JUSTERAD KVANTITET                      
