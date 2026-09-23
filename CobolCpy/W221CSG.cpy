000100 01  W221CSG.                                                             
000200*                                 ODETTE-SEGMENT CSG                      
000300*                                 CONSIGNEE DETAILS                       
000400*                                                                         
000500*                                 FORMAT : HELD-AS                        
000600     03 IDPT                 PIC X(3).                                    
000700*                                 POSTTYP                                 
000710     03 IDPTYP-LTH           PIC X(3) VALUE '070'.                        
000720*                                 LÄNGD PÅ FÄLT                           
000730     03 BEFTAG               PIC X(35).                                   
000740*                                 MOTTAGARE         TAG 3036              
000900     03 KDFORBR              PIC X(10).                                   
001000*                                 SLUTFÖRBRUKARKOD  TAG 3296              
001500     03 KDGODSM              PIC X(8).                                    
001600*                                 GODSMOTTAGN.KOD   TAG 3921              
001700     03 KDGODSM-DET          PIC X(17).                                   
001800*                                 DETALJ. GODSM.KOD TAG 3923              
002400*** END COPY W221CSG     LENGTH=76    OLD LENGTH=76                       
