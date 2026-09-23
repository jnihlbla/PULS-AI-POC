000100 01  MOD-W4O24201.                                                        
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
002700     03 MOD-RADER            OCCURS 14 TIMES.                             
002800        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000        05 MOD-IDARTNR       PIC X(11).                                   
003100*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
003200        05 MOD-KVBEART-ATTR  PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-KVBEART       PIC Z(5)9.                                   
003500*                                 BESTÄLLT ANTAL STYCKEN                  
003600        05 MOD-BERADREF      PIC X(10).                                   
003700*                                 KUNDENS RADREFERENS                     
003800        05 MOD-TITPO-ATTR    PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-TITPO         PIC 9(6).                                    
004100*                                 PLANERAD ORDERDATUM                     
004200        05 MOD-FLRESTN-ATTR  PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-FLRESTN       PIC X.                                       
004500*                                 RESTNOTERING ?                          
004600        05 MOD-FLSLATT-ATTR  PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800        05 MOD-FLSLATT       PIC X.                                       
004900*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
005000*                                 LL BERÄKNAS ELLER EJ                    
005100*                                 OM FLRESTN = J OCH FLSLATT = J,         
005200*                                  DÅ BERÄKNAS KVSLATT                    
005300        05 MOD-KDKVBRYT-ATTR PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500        05 MOD-KDKVBRYT      PIC 9.                                       
005600*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
005700        05 MOD-FLORDING-ATTR PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-FLORDING      PIC X.                                       
006000     03 MOD-FILLER           PIC X.                                       
006100     03 MOD-TEMFSINF         PIC X(55).                                   
006200*                                 INFORMATIONSMEDDELANDE                  
006300*** END OF VILMAII-COPY LENGTH= 868 BYTES                                 
