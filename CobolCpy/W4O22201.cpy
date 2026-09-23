000100 01  MOD-W4O22201-CTX.                                                    
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
001800     03 MOD-FLVORKO          PIC X.                                       
001900*                                 VOR-KÖ FLAGGA                           
002000     03 MOD-FLFORBI          PIC X.                                       
002100*                                 FÖRBIORDERFLAGGA                        
002200     03 MOD-KDTRTYP          PIC X.                                       
002300*                                 IMS TRANSAKTIONSTYP                     
002400     03 MOD-TEDDI            PIC X(11).                                   
002500*                                 TEXTFÄLT DDI                            
002600     03 MOD-KDVALISO         PIC X(3).                                    
002700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002800     03 MOD-W4O22201-GRP     OCCURS 14 TIMES.                             
002900        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-IDARTNR-006   PIC X(11).                                   
003200*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
003300        05 MOD-KVBEART-ATTR  PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-KVBEART       PIC Z(5)9.                                   
003600*                                 BESTÄLLT ANTAL STYCKEN                  
003700        05 MOD-BERADREF      PIC X(10).                                   
003800*                                 KUNDENS RADREFERENS                     
003900        05 MOD-PRARTNTO-ATTR PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100        05 MOD-PRARTNTO      PIC Z(6)9.9(2).                              
004200*                                 ARTIKELPRIS NETTO                       
004300        05 MOD-FLINVEST-ATTR PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-FLINVEST      PIC X.                                       
004600*                                 BYTES INVENTERINGSFLAGGA                
004700        05 MOD-KDVRINFO-ATTR PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-KDVRINFO      PIC 9.                                       
005000*                                 PÅVERKAN I VR/DSP SYSTEM                
005100     03 MOD-STOP             PIC X.                                       
005200     03 MOD-TEMFSINF         PIC X(55).                                   
005300*                                 INFORMATIONSMEDDELANDE                  
005400*** END OF VILMAII-COPY LENGTH= 821 BYTES                                 
