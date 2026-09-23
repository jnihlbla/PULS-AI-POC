000100 01  W5I20601.                                                            
000200*                                 COPYTEXT FÖR MID W5I20601.              
000300*                                 INMATAT OCH FÖREGÅENDE ARTNR:           
000400     03 IDARTNR-IN           PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 IDARTNR-UT           PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 IDLEVNR-IN           PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000     03 IDLEVNR-UT           PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200     03 KDPRBEH-IN           PIC X.                                       
001300*                                 PRIS BEHANDLAD ARTIKEL                  
001400     03 KDPRBEH-UT           PIC X.                                       
001500*                                 PRIS BEHANDLAD ARTIKEL                  
001600     03 REAENDR-IN           PIC X(6).                                    
001700*                                 ÄNDRINGSPROCENT                         
001800     03 REAENDR-UT           PIC X(6).                                    
001900*                                 ÄNDRINGSPROCENT                         
002000     03 IDDC                 PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 UPPDAT-RAD.                                                       
002300        05 KDPRURSP-U        PIC X.                                       
002400*                                 PRISHÄRSTAMNING BESTÄLLNING             
002500        05 TIPRLIST-U        PIC 9(6).                                    
002600*                                 PRISLISTEDATUM (AAMMDD)                 
002700        05 PRARTBEL-U        PIC X(14).                                   
002800*                                 BESTPRIS LEVERANTÖRENS VALUTA           
002900        05 KDVALISO-U        PIC X(3).                                    
003000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003100        05 FLPRIBES-U        PIC X.                                       
003200*                                 SKAPA EJ BESTÄLLNINGSPRIS-INFO          
003300        05 IDLEVNR-U         PIC X(5).                                    
003400*                                 LEVERANTÖRNUMMER                        
003500        05 KDFPKPRI-U        PIC X.                                       
003600*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
003700     03 RETULF               PIC 9(3)V9(4).                               
003800*                                 TULLFAKTOR                              
003900     03 IDUSER               PIC X(8).                                    
004000*                                 ANVÄNDARENS SÄKERHETS ID                
004100*** END OF VILMAII-COPY LENGTH= 90 BYTES                                  
