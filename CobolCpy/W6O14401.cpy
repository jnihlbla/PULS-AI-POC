000100 01  MOD-W6O14401.                                                        
000200*                                 COPYTEXT FOR MOD W6O14401               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDLEVNR-KOLLI-IN PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200     03 MOD-IDLEVNR-KOLLI-UT PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
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
003700     03 MOD-IDARTNR          PIC Z(7)9.                                   
003800*                                 ARTIKELNUMMER                           
003900*                                 PART NUMBER                             
004000     03 MOD-KVAVIS           PIC Z(5)9.                                   
004100*                                 AVISERAT ANTAL                          
004200*                                 QUANTITY NOTIFIED                       
004300     03 MOD-BEART            PIC X(25).                                   
004400*                                 ARTIKELBENÄMNING                        
004500*                                 PART DESCRIPTION                        
004600     03 MOD-KDSORT           PIC X(2).                                    
004700*                                 SORT-KOD                                
004800*                                 UNIT OF MEASURE                         
004900     03 MOD-BEFT             PIC Z9.                                      
005000*                                 FÖRPACKNINGSTYP                         
005100*                                 PACKAGING TYPE                          
005200     03 MOD-KVRAPP           PIC Z(5)9.                                   
005300*                                 DELRAPPORTERAT ANTAL                    
005400*                                 PARTIAL REPORTED QUANTITY               
005500     03 MOD-BEFARLIG-TEXT    PIC X(10).                                   
005600     03 MOD-ADINLOMR-NXT-ATTR                                             
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-ADINLOMR-NXT     PIC X(4).                                    
006000*                                 INLEVERANSOMRÅDE NÄSTA                  
006100*                                 RECEIVING AREA NEXT                     
006200     03 MOD-ADLAGOMR         PIC Z9.                                      
006300*                                 LAGEROMRÅDE                             
006400*                                 AREA                                    
006500     03 MOD-ADGANG           PIC Z9.                                      
006600*                                 GÅNG                                    
006700*                                 AISLE                                   
006800     03 MOD-ADPLATS          PIC Z(4)9.                                   
006900*                                 LAGERPLATSNUMMER                        
007000*                                 LOCATION                                
007100     03 MOD-FILLER           OCCURS 3 TIMES.                              
007200*                                 UPDATE                                  
007300        05 MOD-ADBUFFOMR     PIC Z9.                                      
007400*                                 BUFFERTOMRÅDE                           
007500*                                 BUFFER AREA                             
007600        05 MOD-ADBUFFGANG    PIC Z9.                                      
007700*                                 BUFFERT GÅNG                            
007800        05 MOD-ADBUFFPL      PIC Z(4)9.                                   
007900*                                 BUFFERPLATSNUMMER                       
008000*                                 LOCATION IN BUFFER                      
008100     03 MOD-IDANSTNR-ATTR    PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300     03 MOD-IDANSTNR         PIC X(5).                                    
008400*                                 ANSTÄLLNINGSNUMMER                      
008500*                                 IDENTIFICATION NO EMPLOYEE              
008600     03 MOD-FILLER           OCCURS 8 TIMES.                              
008700*                                 UPDATE                                  
008800        05 MOD-KDCMDVAL-RAD-ATTR                                          
008900                             PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100        05 MOD-KDCMDVAL-RAD  PIC X(3).                                    
009200*                                 GENERELL KOMMANDOKOD                    
009300*                                 GENERAL COMMAND-CODE                    
009400     03 MOD-FILLER           OCCURS 8 TIMES.                              
009500*                                 UPDATE                                  
009600        05 MOD-KVINLART-UPD-ATTR                                          
009700                             PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900        05 MOD-KVINLART-UPD  PIC X(6).                                    
010000*                                 ANTAL I PARTIRAD                        
010100*                                 QTY/LINE IN A LOT                       
010200     03 MOD-FILLER           OCCURS 8 TIMES.                              
010300*                                 UPDATE                                  
010400        05 MOD-ADINLOMR-NXT-UPD-ATTR                                      
010500                             PIC X(2).                                    
010600*                                 MFS ATTRIBUTFÄLT                        
010700        05 MOD-ADINLOMR-NXT-UPD                                           
010800                             PIC X(4).                                    
010900*                                 INLEVERANSOMRÅDE                        
011000*                                 RECEIVING AREA                          
011100     03 MOD-IDRADNR-RAD      OCCURS 8 TIMES                               
011200                             PIC Z(2)9.                                   
011300*                                 RAD INOM ORDER      IDRADNR-002         
011400     03 MOD-KVINLART-RAD     OCCURS 8 TIMES                               
011500                             PIC Z(5)9.                                   
011600*                                 ANTAL I PARTIRAD                        
011700*                                 QTY/LINE IN A LOT                       
011800     03 MOD-ADINLOMR-RAD     OCCURS 8 TIMES                               
011900                             PIC X(4).                                    
012000*                                 INLEVERANSOMRÅDE                        
012100*                                 RECEIVING AREA                          
012200     03 MOD-KDINLSTA-RAD     OCCURS 8 TIMES                               
012300                             PIC X(3).                                    
012400*                                 SYSTEMSTATUS INLEVERANS                 
012500*                                 SYSTEM STATUS RECEIVING                 
012600     03 MOD-IDLEVNR-KOLLI-RAD                                             
012700                             OCCURS 8 TIMES                               
012800                             PIC X(5).                                    
012900*                                 LEVERANTÖRNUMMER                        
013000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
013100     03 MOD-IDOKOLLI-RAD     OCCURS 8 TIMES                               
013200                             PIC Z(8)9.                                   
013300*                                 ODETTE KOLLINUMMER                      
013400*                                 ODETTE CASE NUMBER                      
013500     03 MOD-KVINLART-VOR-RAD OCCURS 8 TIMES                               
013600                             PIC Z(5)9.                                   
013700*                                 ANTAL I PARTIRAD                        
013800*                                 QTY/LINE IN A LOT                       
013900     03 MOD-TEMFSINF         PIC X(55).                                   
014000*                                 INFORMATIONSMEDDELANDE                  
014100*                                 INFORMATION MESSAGE                     
014200*** END OF VILMAII-COPY LENGTH= 697 BYTES                                 
