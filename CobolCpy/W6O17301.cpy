000100 01  MOD-W6O17301.                                                        
000200*                                 MOD-COPYTEXT FÖR W6017300               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 MOD-IDARTNR-UT       PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400*                                 PART NUMBER                             
001500     03 MOD-KVINLART-IN      PIC X(6).                                    
001600*                                 ANTAL I PARTIRAD                        
001700*                                 QTY/LINE IN A LOT                       
001800     03 MOD-KVINLART-UT      PIC X(6).                                    
001900*                                 ANTAL I PARTIRAD                        
002000*                                 QTY/LINE IN A LOT                       
002100     03 MOD-IDLEVNR-IN       PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002400     03 MOD-IDLEVNR-UT       PIC X(5).                                    
002500*                                 LEVERANTÖRNUMMER                        
002600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002700     03 MOD-IDOKOLLI-IN      PIC X(9).                                    
002800*                                 ODETTE KOLLINUMMER                      
002900*                                 ODETTE CASE NUMBER                      
003000     03 MOD-IDOKOLLI-UT      PIC X(9).                                    
003100*                                 ODETTE KOLLINUMMER                      
003200*                                 ODETTE CASE NUMBER                      
003300     03 MOD-IDDC-IN          PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500*                                 WAREHOUSE IDENTIFIER                    
003600     03 MOD-IDDC-UT          PIC X(2).                                    
003700*                                 IDENTIFIERARE LAGER                     
003800*                                 WAREHOUSE IDENTIFIER                    
003900     03 MOD-ADINLOMR-PRT-ATTR                                             
004000                             PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
004300*                                 PRINTERPLACERING                        
004400*                                 PLACE OF A PRINTER                      
004500     03 MOD-ADTRDEST-ATTR    PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-ADTRDEST         PIC X(3).                                    
004800*                                 TRANSPORTDESTINATION                    
004900*                                 ADDRESS OF TRANSPORT                    
005000     03 MOD-KVINLART-ATTR    PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-KVINLART         PIC Z(6).                                    
005300*                                 ANTAL I PARTIRAD                        
005400*                                 QTY/LINE IN A LOT                       
005500     03 MOD-TIINLMOT-ATTR    PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-TIINLMOT         PIC 9(6).                                    
005800*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
005900*                                 RECEIVING DATE    (YYMMDD)              
006000     03 MOD-KVFLETI-ATTR     PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-KVFLETI          PIC Z9.                                      
006300*                                 ANTAL FLAGGOR EL ETIKETTER              
006400*                                 NO OF FLAGS OR LABELS                   
006500     03 MOD-KVINLART-LAST-ATTR                                            
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-KVINLART-LAST    PIC Z(6).                                    
006900*                                 ANTAL I PARTIRAD                        
007000*                                 QTY/LINE IN A LOT                       
007100     03 MOD-TEMFSINF         PIC X(55).                                   
007200*                                 INFORMATIONSMEDDELANDE                  
007300*                                 INFORMATION MESSAGE                     
007400*** END OF VILMAII-COPY LENGTH= 200 BYTES                                 
