000100 01  MOD-W4O26201-CTX.                                                    
000200*                                 MOD AREA FÖR PROFORMARADER              
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 MOD-IDORDNR-IN       PIC X(7).                                    
001300*                                 ORDERNUMMER                             
001400     03 MOD-IDARTNR-IN       PIC X(9).                                    
001500*                                 ARTIKELNUMMER                           
001600     03 MOD-IDDISTR-UT       PIC X(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001900*                                 KUNDNUMMER                              
002000     03 MOD-IDORDNR-UT       PIC X(7).                                    
002100*                                 ORDERNUMMER                             
002200     03 MOD-KDORDKL-UT       PIC X.                                       
002300*                                 ORDERKLASS                              
002400     03 MOD-KDFRAKT-UT       PIC Z9.                                      
002500*                                 FRAKTSÄTT DC TILL KUND                  
002600     03 MOD-KDPROTYP-UT      PIC X.                                       
002700*                                 TYP AV PROFORMA                         
002800     03 MOD-TEDDI            PIC X(11).                                   
002900*                                 TEXTFÄLT DDI                            
003000     03 MOD-KDVALISO         PIC X(3).                                    
003100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003200     03 MOD-W4O26201-GRP     OCCURS 14 TIMES.                             
003300        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-IDARTNR-006   PIC X(11).                                   
003600*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
003700        05 MOD-KVBEART-ATTR  PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-KVBEART       PIC Z(5)9.                                   
004000*                                 BESTÄLLT ANTAL STYCKEN                  
004100        05 MOD-BERADREF      PIC X(10).                                   
004200*                                 KUNDENS RADREFERENS                     
004300        05 MOD-PRARTNTO-ATTR PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-PRARTNTO      PIC Z(6)9.9(2).                              
004600*                                 ARTIKELPRIS NETTO                       
004700        05 MOD-KDKVBRYT-ATTR PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-KDKVBRYT      PIC 9.                                       
005000*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
005100        05 MOD-FLINVEST-ATTR PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 MOD-FLINVEST      PIC X.                                       
005400*                                 BYTES INVENTERINGSFLAGGA                
005500     03 MOD-FILLERX1         PIC X.                                       
005600     03 MOD-TEMFSINF         PIC X(55).                                   
005700*                                 INFORMATIONSMEDDELANDE                  
005800*** END OF VILMAII-COPY LENGTH= 847 BYTES                                 
