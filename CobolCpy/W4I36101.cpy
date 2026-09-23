000100 01  W4I36101.                                                            
000200*                                 COPYTEXT FÖR MID W4I36101               
000300     03 IDPKLTAB-IN          PIC X(2).                                    
000400*                                 PRODUKTIONSKLASSTABELLSID               
000500     03 IDPKLTAB-UT          PIC X(2).                                    
000600*                                 PRODUKTIONSKLASSTABELLSID               
000700     03 IDDC-IN              PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 IDDC-UT              PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 IDRADNR-DOLD         PIC 9(5).                                    
001200*                                 RADNUMMER                               
001300     03 IDRADNR-DOLD-X       PIC 9(5).                                    
001400*                                 RADNUMMER                               
001500     03 IDRADNR-IN           PIC 9(5).                                    
001600*                                 RADNUMMER                               
001700     03 INDATA.                                                           
001800        05 KDORDKL-IN        PIC 9.                                       
001900*                                 ORDERKLASS                              
002000        05 KVRADER-IN        PIC 9(5).                                    
002100*                                 ANTAL RADER                             
002200        05 VKORDNTO-IN       PIC X(8).                                    
002300*                                 ORDERVIKT NETTO (KG)                    
002400        05 VLORDNTO-IN       PIC X(8).                                    
002500*                                 ORDERVOLYM NETTO (M3)                   
002600        05 KDPRODKL-IN       PIC X.                                       
002700*                                 PRODUKTIONSKLASS                        
002800        05 IDHLOTAB-IN       PIC 9(2).                                    
002900*                                 HLOTABELLSIDENTITET                     
003000*** END COPY W4I36101    LENGTH=48                                        
