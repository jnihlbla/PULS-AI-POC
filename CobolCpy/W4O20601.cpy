000100 01  MOD-W4O20601.                                                        
000200*                                 MOD AREA FÖR ORDERRADER                 
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR          PIC Z(3)9.                                   
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDKUNDNR         PIC Z(5)9.                                   
001100*                                 KUNDNUMMER                              
001200     03 MOD-IDORDNR5         PIC Z(4)9.                                   
001300*                                 ORDERNUMMER                             
001400     03 MOD-IDORDNR5-REG     PIC X(5).                                    
001500*                                 ORDERNUMMER                             
001600     03 MOD-KDORDKL          PIC 9.                                       
001700*                                 ORDERKLASS                              
001800     03 MOD-KDFRAKT          PIC Z9.                                      
001900*                                 FRAKTSÄTT DC TILL KUND                  
002000     03 MOD-KDTRTYP          PIC X.                                       
002100*                                 IMS TRANSAKTIONSTYP                     
002200     03 MOD-TEDDI            PIC X(11).                                   
002300*                                 TEXTFÄLT DDI                            
002400     03 MOD-KDVALISO         PIC X(3).                                    
002500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002600     03 MOD-RADER            OCCURS 14 TIMES.                             
002700        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900        05 MOD-IDARTNR       PIC X(11).                                   
003000*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
003100        05 MOD-KVBEART-ATTR  PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-KVBEART       PIC Z(5)9.                                   
003400*                                 BESTÄLLT ANTAL STYCKEN                  
003500        05 MOD-BERADREF-ATTR PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700        05 MOD-BERADREF      PIC X(10).                                   
003800*                                 KUNDENS RADREFERENS                     
003900        05 MOD-PRARTNTO-ATTR PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100        05 MOD-PRARTNTO      PIC Z(6)9.9(2).                              
004200*                                 ARTIKELPRIS NETTO                       
004300        05 MOD-TITPO-ATTR    PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-TITPO         PIC 9(6).                                    
004600*                                 PLANERAD ORDERDATUM                     
004700        05 MOD-FLRESTN-ATTR  PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-FLRESTN       PIC X.                                       
005000*                                 RESTNOTERING ?                          
005100        05 MOD-FLSLATT-ATTR  PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 MOD-FLSLATT       PIC X.                                       
005400*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
005500*                                 LL BERÄKNAS ELLER EJ                    
005600*                                 OM FLRESTN = J OCH FLSLATT = J,         
005700*                                  DÅ BERÄKNAS KVSLATT                    
005800        05 MOD-KDKVBRYT-ATTR PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000        05 MOD-KDKVBRYT      PIC 9.                                       
006100*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
006200        05 MOD-FLINVEST-ATTR PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-FLINVEST      PIC X.                                       
006500*                                 BYTES INVENTERINGSFLAGGA                
006600        05 MOD-KDVRINFO-ATTR PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-KDVRINFO      PIC 9.                                       
006900*                                 PÅVERKAN I VR/DSP SYSTEM                
007000     03 MOD-TEMFSINF         PIC X(55).                                   
007100*                                 INFORMATIONSMEDDELANDE                  
007200*** END OF VILMAII-COPY LENGTH= 1089 BYTES                                
