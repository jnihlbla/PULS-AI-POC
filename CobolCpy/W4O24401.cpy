000100 01  MOD-W4O24401.                                                        
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
002800     03 MOD-RADER            OCCURS 14 TIMES.                             
002900        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-IDARTNR       PIC X(11).                                   
003200*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
003300        05 MOD-KVBEART-ATTR  PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-KVBEART       PIC Z(5)9.                                   
003600*                                 BESTÄLLT ANTAL STYCKEN                  
003700        05 MOD-BERADREF      PIC X(10).                                   
003800*                                 KUNDENS RADREFERENS                     
003900        05 MOD-TITPO-ATTR    PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100        05 MOD-TITPO         PIC 9(6).                                    
004200*                                 PLANERAD ORDERDATUM                     
004300        05 MOD-FLRESTN-ATTR  PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-FLRESTN       PIC X.                                       
004600*                                 RESTNOTERING ?                          
004700        05 MOD-FLSLATT-ATTR  PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-FLSLATT       PIC X.                                       
005000*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
005100*                                 LL BERÄKNAS ELLER EJ                    
005200*                                 OM FLRESTN = J OCH FLSLATT = J,         
005300*                                  DÅ BERÄKNAS KVSLATT                    
005400        05 MOD-KDKVBRYT-ATTR PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-KDKVBRYT      PIC 9.                                       
005700*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
005800     03 MOD-FILLER           PIC X.                                       
005900     03 MOD-TEMFSINF         PIC X(55).                                   
006000*                                 INFORMATIONSMEDDELANDE                  
006100*** END OF VILMAII-COPY LENGTH= 817 BYTES                                 
