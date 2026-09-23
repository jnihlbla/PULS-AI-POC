000100 01  REQU-W40343I2.                                                       
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
001300     03 REQU-KDMATT          PIC X.                                       
001400*                                 MÅTTKOD                                 
001500     03 REQU-KVRADER         PIC 9(5).                                    
001600*                                 ANTAL RADER                             
001700     03 REQU-IDRADNR-FOM     PIC 9(5).                                    
001800*                                 RADNUMMER                               
001900     03 REQU-IDRADNR-TOM     PIC 9(5).                                    
002000*                                 RADNUMMER                               
002100     03 REQU-FLJANEJ-ALLA    PIC X(3).                                    
002200*                                                     FLJANEJ-004         
002300     03 REQU-IDKOLLI-ALLA    PIC X(5).                                    
002400*                                 KOLLINUMMER                             
002500     03 REQU-IDKOLLI-NY      PIC X(5).                                    
002600*                                 KOLLINUMMER                             
002700     03 REQU-KDKOLLI         PIC X(8).                                    
002800*                                 KOLLIKOD                                
002900     03 REQU-KDEMBTYP        PIC X(3).                                    
003000*                                 EMBALLAGETYP       KDEMBTYP-002         
003100     03 REQU-VKORDBTO        PIC X(8).                                    
003200*                                 ORDERVIKT BRUTTO (KG)                   
003300     03 REQU-DIKOLLIL        PIC X(5).                                    
003400*                                 KOLLI-LÄNGD                             
003500     03 REQU-DIKOLLIB        PIC X(3).                                    
003600*                                 KOLLI-BREDD                             
003700     03 REQU-DIKOLLIH        PIC X(3).                                    
003800*                                 KOLLI-HÖJD                              
003900     03 REQU-RAD-GRUPP       OCCURS 500 TIMES.                            
004000*                                                                         
004100        05 REQU-FLNOLLAD-LINE                                             
004200                             PIC X.                                       
004300*                                 KOPPLING BORTTAGEN PÅ REG               
004400        05 REQU-IDARTNR-LINE PIC X(9).                                    
004500*                                 ARTIKELNUMMER                           
004600        05 REQU-IDKOLLI-LINE PIC X(5).                                    
004700*                                 KOLLINUMMER                             
004800        05 REQU-KVLEVART-LINE-IN                                          
004900                             PIC X(7).                                    
005000*                                 LEVERERAT ANTAL STYCK                   
005100*** END OF VILMAII-COPY LENGTH= 11075 BYTES                               
