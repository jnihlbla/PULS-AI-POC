000100 01  REQU-W40343I1.                                                       
000200*                                 REQU-COPYTEXT FÖR MID W40343            
000300*                                                                         
000400*                                                                         
000500     03 REQU-IDPRODNR-KEY    PIC X(7).                                    
000600*                                 PRODUKTIONSNUMMER                       
000700     03 REQU-IDKOLLI-KEY     PIC X(5).                                    
000800*                                 KOLLINUMMER                             
000900     03 REQU-IDDC-KEY        PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 REQU-IDSPRAK         PIC X(2).                                    
001200*                                 2-STÄLLIG ISO SPRÅKKOD                  
001300     03 REQU-KVRADER         PIC 9(5).                                    
001400*                                 ANTAL RADER                             
001500     03 REQU-IDRADNR-FOM     PIC 9(5).                                    
001600*                                 RADNUMMER                               
001700     03 REQU-IDRADNR-TOM     PIC 9(5).                                    
001800*                                 RADNUMMER                               
001900     03 REQU-FLJANEJ-ALLA    PIC X(3).                                    
002000*                                                     FLJANEJ-004         
002100     03 REQU-IDKOLLI-ALLA    PIC X(5).                                    
002200*                                 KOLLINUMMER                             
002300     03 REQU-IDKOLLI-NY      PIC X(5).                                    
002400*                                 KOLLINUMMER                             
002500     03 REQU-KDKOLLI         PIC X(8).                                    
002600*                                 KOLLIKOD                                
002700     03 REQU-KDEMBTYP        PIC X(3).                                    
002800*                                 EMBALLAGETYP       KDEMBTYP-002         
002900     03 REQU-VKORDBTO        PIC X(8).                                    
003000*                                 ORDERVIKT BRUTTO (KG)                   
003100     03 REQU-DIKOLLIL        PIC X(5).                                    
003200*                                 KOLLI-LÄNGD                             
003300     03 REQU-DIKOLLIB        PIC X(3).                                    
003400*                                 KOLLI-BREDD                             
003500     03 REQU-DIKOLLIH        PIC X(3).                                    
003600*                                 KOLLI-HÖJD                              
003700     03 REQU-RAD-GRUPP       OCCURS 500 TIMES.                            
003800*                                                                         
003900        05 REQU-FLNOLLAD-LINE                                             
004000                             PIC X.                                       
004100*                                 KOPPLING BORTTAGEN PÅ REG               
004200        05 REQU-IDARTNR-LINE PIC X(9).                                    
004300*                                 ARTIKELNUMMER                           
004400        05 REQU-IDKOLLI-LINE PIC X(5).                                    
004500*                                 KOLLINUMMER                             
004600        05 REQU-KVLEVART-LINE-IN                                          
004700                             PIC X(7).                                    
004800*                                 LEVERERAT ANTAL STYCK                   
004900*** END OF VILMAII-COPY LENGTH= 11074 BYTES                               
