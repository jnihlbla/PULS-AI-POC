000100 01  MOD-W4O20201.                                                        
000200*                                 MOD AREA FÖR ORDERRADER                 
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 MOD-IDORDNR-IN       PIC X(5).                                    
001300*                                 ORDERNUMMER                             
001400     03 MOD-IDDISTR-UT       PIC X(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001700*                                 KUNDNUMMER                              
001800     03 MOD-IDORDNR-UT       PIC X(5).                                    
001900*                                 ORDERNUMMER                             
002000     03 MOD-KDORDKL-UT       PIC X.                                       
002100*                                 ORDERKLASS                              
002200     03 MOD-KDFRAKT-UT       PIC Z9.                                      
002300*                                 FRAKTSÄTT DC TILL KUND                  
002400     03 MOD-KDTRTYP          PIC X.                                       
002500*                                 IMS TRANSAKTIONSTYP                     
002600     03 MOD-TEDDI            PIC X(11).                                   
002700*                                 TEXTFÄLT DDI                            
002800     03 MOD-KDVALISO         PIC X(3).                                    
002900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003000     03 MOD-RADER            OCCURS 14 TIMES.                             
003100        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-IDARTNR       PIC X(11).                                   
003400*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
003500        05 MOD-KVBEART-ATTR  PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700        05 MOD-KVBEART       PIC Z(5)9.                                   
003800*                                 BESTÄLLT ANTAL STYCKEN                  
003900        05 MOD-BERADREF      PIC X(10).                                   
004000*                                 KUNDENS RADREFERENS                     
004100        05 MOD-PRARTNTO-ATTR PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-PRARTNTO      PIC Z(6)9.9(2).                              
004400*                                 ARTIKELPRIS NETTO                       
004500        05 MOD-TITPO-ATTR    PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-TITPO         PIC 9(6).                                    
004800*                                 PLANERAD ORDERDATUM                     
004900        05 MOD-FLRESTN-ATTR  PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-FLRESTN       PIC X.                                       
005200*                                 RESTNOTERING ?                          
005300        05 MOD-FLSLATT-ATTR  PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500        05 MOD-FLSLATT       PIC X.                                       
005600*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
005700*                                 LL BERÄKNAS ELLER EJ                    
005800*                                 OM FLRESTN = J OCH FLSLATT = J,         
005900*                                  DÅ BERÄKNAS KVSLATT                    
006000        05 MOD-KDKVBRYT-ATTR PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 MOD-KDKVBRYT      PIC 9.                                       
006300*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
006400        05 MOD-FLINVEST-ATTR PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-FLINVEST      PIC X.                                       
006700*                                 BYTES INVENTERINGSFLAGGA                
006800        05 MOD-KDVRINFO-ATTR PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-KDVRINFO      PIC 9.                                       
007100*                                 PÅVERKAN I VR/DSP SYSTEM                
007200     03 MOD-STOP             PIC X.                                       
007300     03 MOD-TEMFSINF         PIC X(55).                                   
007400*                                 INFORMATIONSMEDDELANDE                  
007500*** END OF VILMAII-COPY LENGTH= 1072 BYTES                                
