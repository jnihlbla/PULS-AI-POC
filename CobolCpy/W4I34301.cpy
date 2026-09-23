000100 01  W4I34301.                                                            
000200*                                 COPYTEXT FÖR MID W4I34301               
000300*                                                                         
000400     03 IDPRODNR-IN          PIC X(7).                                    
000500*                                 PRODUKTIONSNUMMER                       
000600     03 IDKOLLI-IN           PIC X(5).                                    
000700*                                 KOLLINUMMER                             
000800     03 IDDC-IN              PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 IDPRODNR-UT          PIC X(7).                                    
001100*                                 PRODUKTIONSNUMMER                       
001200     03 IDKOLLI-UT           PIC X(5).                                    
001300*                                 KOLLINUMMER                             
001400     03 IDDC-UT              PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 IDRADNR-FOM          PIC 9(5).                                    
001700*                                 RADNUMMER                               
001800     03 IDRADNR-TOM          PIC 9(5).                                    
001900*                                 RADNUMMER                               
002000     03 FLNOLLAD-TAB.                                                     
002100*                                                                         
002200        05 FLNOLLAD          OCCURS 16 TIMES                              
002300                             PIC X.                                       
002400*                                 KOPPLING BORTTAGEN PÅ REG               
002500     03 FLJANEJ-ALLA         PIC X(3).                                    
002600*                                                     FLJANEJ-004         
002700     03 IDKOLLI-ALLA         PIC X(5).                                    
002800*                                 KOLLINUMMER                             
002900     03 RAD-GRUPP            OCCURS 16 TIMES.                             
003000*                                                                         
003100        05 IDARTNR           PIC X(9).                                    
003200*                                 ARTIKELNUMMER                           
003300        05 IDKOLLI-RAD       PIC X(5).                                    
003400*                                 KOLLINUMMER                             
003500        05 KVLEVART          PIC X(7).                                    
003600*                                 LEVERERAT ANTAL STYCK                   
003700     03 IDKOLLI-NY           PIC X(5).                                    
003800*                                 KOLLINUMMER                             
003900     03 KDKOLLI              PIC X(8).                                    
004000*                                 KOLLIKOD                                
004100     03 KDEMBTYP             PIC X(3).                                    
004200*                                 EMBALLAGETYP       KDEMBTYP-002         
004300     03 VKORDBTO             PIC X(8).                                    
004400*                                 ORDERVIKT BRUTTO (KG)                   
004500     03 DIKOLLIL             PIC X(5).                                    
004600*                                 KOLLI-LÄNGD                             
004700     03 DIKOLLIB             PIC X(3).                                    
004800*                                 KOLLI-BREDD                             
004900     03 DIKOLLIH             PIC X(3).                                    
005000*                                 KOLLI-HÖJD                              
005100*** END OF VILMAII-COPY LENGTH= 433 BYTES                                 
