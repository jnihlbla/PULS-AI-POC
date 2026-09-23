000100 01  MOD-W6O14501.                                                        
000200*                                 COPYTEXT FOR MOD W6O14501               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDLEVNR-KOLLI-IN PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER                         
001200     03 MOD-IDLEVNR-KOLLI-UT PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400*                                 SUPPLIER NUMBER                         
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
003700     03 MOD-FLKLAR-ATTR      PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-FLKLAR           PIC X.                                       
004000*                                 AVSLUTNINGSMARKERING                    
004100*                                 FINISHED FLAG                           
004200     03 MOD-IDANSTNR-ATTR    PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-IDANSTNR         PIC Z(4)9.                                   
004500*                                 ANSTÄLLNINGSNUMMER                      
004600*                                 IDENTIFICATION NO EMPLOYEE              
004700     03 MOD-FILLER           OCCURS 12 TIMES.                             
004800*                                 UPDATE                                  
004900        05 MOD-KDCMDVAL-RAD-ATTR                                          
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-KDCMDVAL-RAD  PIC X(3).                                    
005300*                                 GENERELL KOMMANDOKOD                    
005400*                                 GENERAL COMMAND-CODE                    
005500     03 MOD-FILLER           OCCURS 12 TIMES.                             
005600*                                 UPDATE                                  
005700        05 MOD-KVINLART-UPD-ATTR                                          
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000        05 MOD-KVINLART-UPD  PIC X(6).                                    
006100*                                 ANTAL I PARTIRAD                        
006200*                                 QTY/LINE IN A LOT                       
006300     03 MOD-FILLER           OCCURS 12 TIMES.                             
006400*                                 UPDATE                                  
006500        05 MOD-ADINLOMR-NXT-UPD-ATTR                                      
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-ADINLOMR-NXT-UPD                                           
006900                             PIC X(4).                                    
007000*                                 INLEVERANSOMRÅDE                        
007100*                                 RECEIVING AREA                          
007200     03 MOD-FILLER           OCCURS 12 TIMES.                             
007300*                                 RAD                                     
007400        05 MOD-ADLAGOMR-RAD-ATTR                                          
007500                             PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700        05 MOD-ADLAGOMR-RAD  PIC Z9.                                      
007800*                                 LAGEROMRÅDE                             
007900*                                 AREA                                    
008000     03 MOD-FILLER           OCCURS 12 TIMES.                             
008100*                                 RAD                                     
008200        05 MOD-ADGANG-RAD-ATTR                                            
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500        05 MOD-ADGANG-RAD    PIC Z9.                                      
008600*                                 GÅNG                                    
008700*                                 AISLE                                   
008800     03 MOD-FILLER           OCCURS 12 TIMES.                             
008900*                                 RAD                                     
009000        05 MOD-ADPLATS-RAD-ATTR                                           
009100                             PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300        05 MOD-ADPLATS-RAD   PIC Z(4)9.                                   
009400*                                 LAGERPLATSNUMMER                        
009500*                                 LOCATION                                
009600     03 MOD-FILLER           OCCURS 12 TIMES.                             
009700*                                 RAD                                     
009800        05 MOD-IDARTNR-RAD-ATTR                                           
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100        05 MOD-IDARTNR-RAD   PIC Z(7)9.                                   
010200*                                 ARTIKELNUMMER                           
010300*                                 PART NUMBER                             
010400     03 MOD-BEART-RAD        OCCURS 12 TIMES                              
010500                             PIC X(10).                                   
010600     03 MOD-KVINLART-RAD     OCCURS 12 TIMES                              
010700                             PIC Z(5)9.                                   
010800*                                 ANTAL I PARTIRAD                        
010900*                                 QTY/LINE IN A LOT                       
011000     03 MOD-KVINLART-VOR-RAD OCCURS 12 TIMES                              
011100                             PIC Z(5)9.                                   
011200*                                 ANTAL I PARTIRAD                        
011300*                                 QTY/LINE IN A LOT                       
011400     03 MOD-IDLOPNRM-RAD     OCCURS 12 TIMES                              
011500                             PIC Z(8)9.                                   
011600*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
011700*                                 (0VVDLLLLK)                             
011800*                                 SERIAL NO RECEIVING REPORT              
011900*                                 (0WWDLLLLC)                             
012000     03 MOD-IDRADNR-RAD      OCCURS 12 TIMES                              
012100                             PIC Z(2)9.                                   
012200*                                 RAD INOM ORDER      IDRADNR-002         
012300     03 MOD-FILLER           OCCURS 12 TIMES.                             
012400*                                 RAD                                     
012500        05 MOD-BEFARLIG-RAD-ATTR                                          
012600                             PIC X(2).                                    
012700*                                 MFS ATTRIBUTFÄLT                        
012800        05 MOD-BEFARLIG-RAD  PIC X.                                       
012900     03 MOD-FILLER           OCCURS 12 TIMES.                             
013000*                                 RAD                                     
013100        05 MOD-KDKLIPRI-RAD-ATTR                                          
013200                             PIC X(2).                                    
013300*                                 MFS ATTRIBUTFÄLT                        
013400        05 MOD-KDKLIPRI-RAD  PIC X.                                       
013500*                                 PRIORITETSKOD KOLLI                     
013600*                                 PRIORITYCODE CASE                       
013700     03 MOD-TEMFSINF         PIC X(55).                                   
013800*                                 INFORMATIONSMEDDELANDE                  
013900*                                 INFORMATION MESSAGE                     
