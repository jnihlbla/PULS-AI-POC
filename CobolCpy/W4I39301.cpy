000100 01  MID-W4I39301.                                                        
000200*                                 MID-COPYTEXT FÖR W40393                 
000300     03 MID-IDPRODNR-IN      PIC X(7).                                    
000400*                                 PRODUKTIONSNUMMER                       
000500     03 MID-IDPRODNR-UT      PIC X(7).                                    
000600*                                 PRODUKTIONSNUMMER                       
000700     03 MID-IDDISTR-UT       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-KDFRAKT-UT       PIC X(2).                                    
001200*                                 FRAKTSÄTT DC TILL KUND                  
001300     03 MID-IDORDNR-UT       PIC X(5).                                    
001400*                                 ORDERNUMMER UTGÅR PD90                  
001500     03 MID-KDORDKL-UT       PIC X.                                       
001600*                                 ORDERKLASS                              
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-PRTVAL-ADRESSFL  PIC X(2).                                    
002000*                                 PRINTER-VAL KOD                         
002100     03 MID-FLETTKOLLI       PIC X.                                       
002200*                                 FLAGGA FÖR ORDER I ETT KOLLI            
002300     03 MID-KVORDRAD         PIC 9(5).                                    
002400*                                 ANTAL ORDERRADER                        
002500     03 MID-IDRADNR-SENAST   PIC X(4).                                    
002600*                                 RADNUMMER                               
002700     03 MID-RAPP-RAD.                                                     
002800*                                 MID-COPYTEXT FÖR W40393                 
002900        05 MID-RAD           OCCURS 13 TIMES.                             
003000*                                 MID-COPYTEXT FÖR W40393                 
003100           07 MID-IDRADNR-FOM                                             
003200                             PIC 9(4).                                    
003300*                                 RADNUMMER                               
003400           07 MID-IDRADNR-TOM                                             
003500                             PIC 9(4).                                    
003600*                                 RADNUMMER                               
003700           07 MID-IDKOLLI    PIC 9(5).                                    
003800*                                 KOLLINUMMER                             
003900           07 MID-KVLEVART   PIC 9(6).                                    
004000*                                 LEVERERAT ANTAL STYCK                   
004100           07 MID-FLRADDEL   PIC X.                                       
004200*                                 FLAGGA DELNING AV RAD                   
004300*** END OF VILMAII-COPY LENGTH= 306 BYTES                                 
