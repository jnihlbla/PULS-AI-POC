000100 01  MID-W5I11401.                                                        
000200*                                 MID-COPY TEXT FÖR W5011400              
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MID-IDLEVNR-UT       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MID-KDPRBEH-IN       PIC X.                                       
001200*                                 PRIS BEHANDLAD ARTIKEL                  
001300     03 MID-KDPRBEH-UT       PIC X.                                       
001400*                                 PRIS BEHANDLAD ARTIKEL                  
001500     03 MID-REAENDR-IN       PIC X(6).                                    
001600*                                 ÄNDRINGSPROCENT                         
001700     03 MID-REAENDR-UT       PIC X(6).                                    
001800*                                 ÄNDRINGSPROCENT                         
001900     03 MID-IDARTNR-ENTER    PIC X(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MID-IDLEVNR-ENTER    PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300     03 MID-FLAGGA           PIC X.                                       
002400*                                 ALLMÄN FLAGGA                           
002500     03 MID-PRINK-KOM        PIC X(10).                                   
002600*                                 INKÖPSPRIS NÄSTA ÅR                     
002700     03 MID-REAENDR          PIC X(7).                                    
002800*                                 ÄNDRINGSPROCENT                         
002900     03 MID-INPUT.                                                        
003000*                                 RADINFORMATION                          
003100        05 MID-RETULF-IN     PIC X(8).                                    
003200*                                 TULLFAKTOR                              
003300        05 MID-PRKURS-IN     PIC X(12).                                   
003400*                                 VALUTAKURS                              
003500        05 MID-TIPRLIST-INM  PIC X(6).                                    
003600*                                 PRISLISTEDATUM (AAMMDD)                 
003700        05 MID-IDLEVNR-INM   PIC X(5).                                    
003800*                                 LEVERANTÖRNUMMER                        
003900        05 MID-PRARTBEL-PR-INM                                            
004000                             PIC X(14).                                   
004100*                                 BESTPRIS LEVERANTÖRENS VALUTA           
004200        05 MID-KDVALISO-INM  PIC X(3).                                    
004300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004400        05 MID-KDPRBEH-INM   PIC X.                                       
004500*                                 PRIS BEHANDLAD ARTIKEL                  
004600        05 MID-TEARTNOT-IN-UT                                             
004700                             PIC X(40).                                   
004800*                                 ARTIKEL NOTERING                        
004900        05 MID-FLSVAR-UPPDAT PIC X.                                       
005000*                                 ALLMÄN FLAGGA                           
005100*** END OF VILMAII-COPY LENGTH= 164 BYTES                                 
