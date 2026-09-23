000100 01  MOD-W6O14301.                                                        
000200*                                 MOD-COPYTEXT FÖR W6014300               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDLEVNR-KOLLI-IN PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER KOLLI                  
001100*                                 SUPPLIER NUMBER CASE                    
001200     03 MOD-IDLEVNR-KOLLI-UT PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER KOLLI                  
001400*                                 SUPPLIER NUMBER CASE                    
001500     03 MOD-IDOKOLLI-IN      PIC X(9).                                    
001600*                                 ODETTE KOLLINUMMER                      
001700*                                 ODETTE CASE NUMBER                      
001800     03 MOD-IDOKOLLI-UT      PIC X(9).                                    
001900*                                 ODETTE KOLLINUMMER                      
002000*                                 ODETTE CASE NUMBER                      
002100     03 MOD-IDLOPNRM-IN      PIC X(9).                                    
002200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002300*                                 (0VVDLLLLK)                             
002400*                                 SERIAL NO RECEIVING REPORT              
002500*                                 (0WWDLLLLC)                             
002600     03 MOD-IDLOPNRM-UT      PIC X(9).                                    
002700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002800*                                 (0VVDLLLLK)                             
002900*                                 SERIAL NO RECEIVING REPORT              
003000*                                 (0WWDLLLLC)                             
003100     03 MOD-IDDC-IN          PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300*                                 WAREHOUSE IDENTIFIER                    
003400     03 MOD-IDDC-UT          PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600*                                 WAREHOUSE IDENTIFIER                    
003700     03 MOD-RADER1.                                                       
003800*                                 GRUPP MED RADER                         
003900        05 MOD-IDARTNR       PIC Z(8).                                    
004000*                                 ARTIKELNUMMER                           
004100*                                 PART NUMBER                             
004200        05 MOD-BEART         PIC X(25).                                   
004300*                                 ARTIKELBENÄMNING                        
004400*                                 PART DESCRIPTION                        
004500        05 MOD-KDSORT        PIC X(2).                                    
004600*                                 SORT-KOD                                
004700*                                 UNIT OF MEASURE                         
004800        05 MOD-BEFT          PIC Z9.                                      
004900*                                 FÖRPACKNINGSTYP                         
005000*                                 PACKAGING TYPE                          
005100        05 MOD-KDFARLIG      PIC X(10).                                   
005200        05 MOD-ADINLOMR-FB-ATTR                                           
005300                             PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500        05 MOD-ADINLOMR-FB   PIC X(4).                                    
005600*                                 FÖRBEHANDLINGSOMRÅDE.                   
005700*                                 HANDLING AREA                           
005800        05 MOD-KVINLART      PIC Z(5)9.                                   
005900*                                 ANTAL I PARTIRAD                        
006000*                                 QTY/LINE IN A LOT                       
006100        05 MOD-KVINLART-VOR  PIC Z(5)9.                                   
006200*                                 ANTAL I PARTIRAD                        
006300*                                 QTY/LINE IN A LOT                       
006400        05 MOD-ADLAGOMR-ATTR PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-ADLAGOMR      PIC Z9.                                      
006700*                                 LAGEROMRÅDE                             
006800*                                 AREA                                    
006900        05 MOD-ADGANG-ATTR   PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100        05 MOD-ADGANG        PIC Z9.                                      
007200*                                 GÅNG                                    
007300*                                 AISLE                                   
007400        05 MOD-ADPLATS-ATTR  PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600        05 MOD-ADPLATS       PIC Z(4)9.                                   
007700*                                 LAGERPLATSNUMMER                        
007800*                                 LOCATION                                
007900        05 MOD-RADER         OCCURS 3 TIMES.                              
008000*                                 GRUPP MED RADER                         
008100           07 MOD-ADBUFFOMR  PIC Z9.                                      
008200*                                 BUFFERTOMRÅDE                           
008300*                                 BUFFER AREA                             
008400           07 MOD-ADBUFFGANG PIC Z9.                                      
008500*                                 BUFFERT GÅNG                            
008600           07 MOD-ADBUFFPL   PIC Z(4)9.                                   
008700*                                 BUFFERPLATSNUMMER                       
008800*                                 LOCATION IN BUFFER                      
008900     03 MOD-INDATA2.                                                      
009000*                                 INDATAFÄLT                              
009100        05 MOD-KVINLART-DIN-UPP-ATTR                                      
009200                             PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400        05 MOD-KVINLART-DIN-UPP                                           
009500                             PIC X(6).                                    
009600*                                 ANTAL I PARTIRAD                        
009700*                                 QTY/LINE IN A LOT                       
009800        05 MOD-KVINLART-UPP-ATTR                                          
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100        05 MOD-KVINLART-UPP  PIC X(6).                                    
010200*                                 ANTAL I PARTIRAD                        
010300*                                 QTY/LINE IN A LOT                       
010400        05 MOD-IDANSTNR-UPP-ATTR                                          
010500                             PIC X(2).                                    
010600*                                 MFS ATTRIBUTFÄLT                        
010700        05 MOD-IDANSTNR-UPP  PIC X(5).                                    
010800*                                 ANSTÄLLNINGSNUMMER                      
010900*                                 IDENTIFICATION NO EMPLOYEE              
011000        05 MOD-ADINLOMR-NXT-UPP-ATTR                                      
011100                             PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300        05 MOD-ADINLOMR-NXT-UPP                                           
011400                             PIC X(4).                                    
011500*                                 INLEVERANSOMRÅDE NÄSTA                  
011600*                                 RECEIVING AREA NEXT                     
011700     03 MOD-TEMFSINF         PIC X(55).                                   
011800*                                 INFORMATIONSMEDDELANDE                  
011900*                                 INFORMATION MESSAGE                     
012000*** END OF VILMAII-COPY LENGTH= 285 BYTES                                 
