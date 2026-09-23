000100 01  SAP-W411SAP.                                                         
000200*                                 LÄNKAREA TILL W411SAP.                  
000300*                                 KONTROLL AV FÖRTAGSKOD,                 
000400*                                             KONTO,                      
000500*                                             KOSTNADSSTÄLLE,             
000600*                                             RESULTATENHET,              
000700*                                             ANALYSNR                    
000800*                                                                         
000900     03 SAP-IDDISTR          PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100     03 SAP-IDFTG            PIC 9(2).                                    
001200*                                 FÖRETAGSID EKONOM REDOVISNING           
001300     03 SAP-IDFTG-OK         PIC X.                                       
001400     03 SAP-IDKONTO          PIC 9(10).                                   
001500*                                 KONTO                                   
001600     03 SAP-IDKONTO-OK       PIC X.                                       
001700     03 SAP-IDKST            PIC X(10).                                   
001800*                                 KOSTNADSSTÄLLE                          
001900     03 SAP-IDKST-OK         PIC X.                                       
002000     03 SAP-IDPROFIT         PIC X(10).                                   
002100*                                 RESULTATENHET                           
002200     03 SAP-IDPROFIT-OK      PIC X.                                       
002300     03 SAP-IDANALYS         PIC X(12).                                   
002400*                                 ANALYSNUMMER                            
002500     03 SAP-IDANALYS-OK      PIC X.                                       
002600     03 SAP-KDCALL           PIC S9(3)           COMP-3.                  
002700*                                 ANROPSTYP                               
002800     03 SAP-KDFAKTYP         PIC X.                                       
002900*                                 FAKTURATYP                              
003000     03 SAP-KDTRADP          PIC X(4).                                    
003100*                                 TRADING PARTNER                         
003200     03 SAP-BEFEL            PIC X(50).                                   
003300*                                 FELTEXT                                 
003400     03 SAP-IDMFSFEL         PIC X(3).                                    
003500*                                 MFS FELMEDDELANDE NUMMER                
003600*** END OF VILMAII-COPY LENGTH= 112 BYTES                                 
