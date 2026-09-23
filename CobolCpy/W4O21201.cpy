000100 01  MOD-W4O21201.                                                        
000200*                                 MOD AREA FÖR ORDERRADER                 
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR          PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDKUNDNR         PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 MOD-IDORDNR5         PIC X(5).                                    
001300*                                 ORDERNUMMER                             
001400     03 MOD-KDORDKL          PIC X.                                       
001500*                                 ORDERKLASS                              
001600     03 MOD-KDFRAKT          PIC Z9.                                      
001700*                                 FRAKTSÄTT DC TILL KUND                  
001800     03 MOD-KDTRTYP          PIC X.                                       
001900*                                 IMS TRANSAKTIONSTYP                     
002000     03 MOD-BEVOLREF         PIC X(10).                                   
002100*                                 VOLVO REFERENS                          
002200     03 MOD-FLTILLK          OCCURS 14 TIMES                              
002300                             PIC X.                                       
002400*                                 TILLKOMMANDE ARTIKEL ?                  
002500     03 MOD-TEDDI            PIC X(11).                                   
002600*                                 TEXTFÄLT DDI                            
002700     03 MOD-KDVALISO         PIC X(3).                                    
002800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002900     03 MOD-RADER            OCCURS 14 TIMES.                             
003000        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-IDARTNR       PIC X(11).                                   
003300*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
003400        05 MOD-KVBEART-ATTR  PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-KVBEART       PIC Z(5)9.                                   
003700*                                 BESTÄLLT ANTAL STYCKEN                  
003800        05 MOD-BERADREF      PIC X(10).                                   
003900*                                 KUNDENS RADREFERENS                     
004000        05 MOD-PRARTNTO-ATTR PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200        05 MOD-PRARTNTO      PIC Z(6)9.9(2).                              
004300*                                 ARTIKELPRIS NETTO                       
004400        05 MOD-TITPO-ATTR    PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 MOD-TITPO         PIC 9(6).                                    
004700*                                 PLANERAD ORDERDATUM                     
004800        05 MOD-FLRESTN-ATTR  PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000        05 MOD-FLRESTN       PIC X.                                       
005100*                                 RESTNOTERING ?                          
005200        05 MOD-FLSLATT-ATTR  PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-FLSLATT       PIC X.                                       
005500*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
005600*                                 LL BERÄKNAS ELLER EJ                    
005700*                                 OM FLRESTN = J OCH FLSLATT = J,         
005800*                                  DÅ BERÄKNAS KVSLATT                    
005900        05 MOD-KDKVBRYT-ATTR PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-KDKVBRYT      PIC 9.                                       
006200*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
006300        05 MOD-FLINVEST-ATTR PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-FLINVEST      PIC X.                                       
006600*                                 BYTES INVENTERINGSFLAGGA                
006700        05 MOD-FLORDING-ATTR PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-FLORDING      PIC X.                                       
007000        05 MOD-KDVRINFO-ATTR PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200        05 MOD-KDVRINFO      PIC 9.                                       
007300*                                 PÅVERKAN I VR/DSP SYSTEM                
007400     03 MOD-FILLER           PIC X.                                       
007500     03 MOD-TEMFSINF         PIC X(55).                                   
007600*                                 INFORMATIONSMEDDELANDE                  
007700*** END OF VILMAII-COPY LENGTH= 1123 BYTES                                
