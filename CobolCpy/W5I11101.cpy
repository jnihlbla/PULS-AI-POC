000100 01  W5I11101.                                                            
000200*                                 COPYTEXT FÖR MID W5I11101.              
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
002000     03 RETULF-LEV           PIC 9(3)V9(4).                               
002100*                                 TULLFAKTOR                              
002200     03 RETULF               PIC X(8).                                    
002300*                                 TULLFAKTOR                              
002400     03 KDVALISO             PIC X(3).                                    
002500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002600     03 REDIRLEV             PIC X(4).                                    
002700*                                 DIREKTLEVERANSANDEL                     
002800     03 UPPDAT-RAD.                                                       
002900        05 KDPRURSP-U        PIC X.                                       
003000*                                 PRISHÄRSTAMNING BESTÄLLNING             
003100        05 TIPRLIST-U        PIC 9(6).                                    
003200*                                 PRISLISTEDATUM (AAMMDD)                 
003300        05 PRARTBEL-U        PIC X(14).                                   
003400*                                 BESTPRIS LEVERANTÖRENS VALUTA           
003500        05 KDVALISO-U        PIC X(3).                                    
003600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003700        05 PRLFKST-U         PIC X(6).                                    
003800*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
003900        05 FLPRIBES-U        PIC X.                                       
004000*                                 SKAPA EJ BESTÄLLNINGSPRIS-INFO          
004100        05 IDLEVNR-U         PIC X(5).                                    
004200*                                 LEVERANTÖRNUMMER                        
004300        05 TEARTNOT-U        PIC X(40).                                   
004400*                                 ARTIKEL NOTERING                        
004500        05 FLPRIGO-U         PIC X.                                       
004600*                                 STOR PRISÖKNING GODKÄND                 
004700        05 KDFPKPRI-U        PIC X.                                       
004800*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
004900     03 IDUSER               PIC X(8).                                    
005000*                                 ANVÄNDARENS SÄKERHETS ID                
005100*** END OF VILMAII-COPY LENGTH= 150 BYTES                                 
