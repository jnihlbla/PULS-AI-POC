000100 01  REQU-W40332I1.                                                       
000200*                                 REQU-COPYTEXT FÖR MID W40332            
000300*                                                                         
000400*                                                                         
000500     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 REQU-IDORDNR-KEY     PIC 9(5).                                    
001000*                                 ORDERNUMMER UTGÅR PD90                  
001100     03 REQU-IDKOLLI-KEY     PIC 9(5).                                    
001200*                                 KOLLINUMMER                             
001300     03 REQU-IDDC-KEY        PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 REQU-KDMATT          PIC X.                                       
001600*                                 MÅTTKOD                                 
001700     03 REQU-KDKOLLI         PIC X(8).                                    
001800*                                 KOLLIKOD                                
001900     03 REQU-KDEMBTYP        PIC 9.                                       
002000*                                 EMBALLAGETYP                            
002100     03 REQU-DIKOLLIL        PIC 9(4).                                    
002200*                                 KOLLI-LÄNGD                             
002300     03 REQU-DIKOLLIB        PIC 9(3).                                    
002400*                                 KOLLI-BREDD                             
002500     03 REQU-DIKOLLIH        PIC 9(3).                                    
002600*                                 KOLLI-HÖJD                              
002700     03 REQU-VKORDBTO        PIC X(8).                                    
002800*                                 ORDERVIKT BRUTTO (KG)                   
002900*** END OF VILMAII-COPY LENGTH= 50 BYTES                                  
