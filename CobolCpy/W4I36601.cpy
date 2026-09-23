000100 01  MID-W4I36601.                                                        
000200*                                 MID-COPYTEXT FÖR W4036600               
000300     03 MID-IDPRCTAB-IN      PIC X(2).                                    
000400*                                 PRCTABELLIDENTITET                      
000500     03 MID-IDPRCTAB-UT      PIC X(2).                                    
000600*                                 PRCTABELLIDENTITET                      
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-IDRADNR-ENTER    PIC 9(4).                                    
001200*                                 RADNUMMER                               
001300     03 MID-IDRADNR-NEXT     PIC 9(4).                                    
001400*                                 RADNUMMER                               
001500     03 MID-IDRADNR-IN       PIC 9(4).                                    
001600*                                 RADNUMMER                               
001700     03 MID-INPUT.                                                        
001800*                                 INDATA FÖR UPPDATERING                  
001900        05 MID-IDGMTOMR-FOM-UPP                                           
002000                             PIC 9(4).                                    
002100*                                 GODSMOTTAGAREOMRÅDE FRÅN                
002200        05 MID-IDGMTOMR-TOM-UPP                                           
002300                             PIC 9(4).                                    
002400*                                 GODSMOTTAGAREOMRÅDE TILL                
002500        05 MID-KDPRODKL-FOM-UPP                                           
002600                             PIC X.                                       
002700*                                 PRODUKTIONSKLASS FRÅN                   
002800        05 MID-KDPRODKL-TOM-UPP                                           
002900                             PIC X.                                       
003000*                                 PRODUKTIONSKLASS TILL                   
003100        05 MID-KDFRAKT-FOM-UPP                                            
003200                             PIC 9(2).                                    
003300*                                 FRAKTSÄTT FRÅN OCH MED                  
003400        05 MID-KDFRAKT-TOM-UPP                                            
003500                             PIC 9(2).                                    
003600*                                 FRAKTSÄTT TILL OCH MED                  
003700        05 MID-IDTRP-UPP.                                                 
003800*                                 TRANSPORTIDENTITET                      
003900           07 MID-IDTRPLOS   PIC X(3).                                    
004000*                                 TRANSPORTLÖSNING                        
004100           07 MID-IDTRPVAR   PIC X(2).                                    
004200*                                 TRANSPORTLÖSNINGSGRUPP                  
004300        05 MID-IDHLO-FOM-UPP PIC 9(2).                                    
004400*                                 HUVUDLAGEROMRÅDE FRÅN OCH MED           
004500        05 MID-IDHLO-TOM-UPP PIC 9(2).                                    
004600*                                 HUVUDLAGEROMRÅDE TILL OCH MED           
004700        05 MID-IDPTIDTAB-UPP PIC 9(2).                                    
004800*                                 PRODUKTIONSTIDTABELLSIDENTITET          
004900        05 MID-IDPRC-UPP.                                                 
005000*                                 PRODUKTIONSKANAL                        
005100           07 MID-IDPRCBAS   PIC X(3).                                    
005200*                                 PRC-BAS                                 
005300           07 MID-IDPRCVAR   PIC X.                                       
005400*                                 PRC-VARIANT                             
005500*** END COPY W4I36601    LENGTH=49                                        
