000100 01  MOD-W6O21501.                                                        
000200*                                 COPYTEXT FÖR MOD W6021501               
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDARTNR-IN       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300     03 MOD-IDARTNR-UT       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600     03 MOD-IDKVAINF-IN      PIC X(2).                                    
001700*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
001800*                                 LINENO FOR QUALITY CONTROL TEXT         
001900     03 MOD-IDKVAINF-UT      PIC X(2).                                    
002000*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
002100*                                 LINENO FOR QUALITY CONTROL TEXT         
002200     03 MOD-TIREGDAT-IN      PIC X(6).                                    
002300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002400*                                 REGISTRATION DATE (YYMMDD)              
002500     03 MOD-TIREGDAT-UT      PIC X(6).                                    
002600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002700*                                 REGISTRATION DATE (YYMMDD)              
002800     03 MOD-IDDC-IN          PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000*                                 WAREHOUSE IDENTIFIER                    
003100     03 MOD-IDDC-UT          PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300*                                 WAREHOUSE IDENTIFIER                    
003400     03 MOD-SHOW-ALL-IN      PIC X.                                       
003500*                                 ALLMÄN FLAGGA                           
003600*                                 GENERAL FLAG                            
003700     03 MOD-SHOW-ALL-UT      PIC X.                                       
003800*                                 ALLMÄN FLAGGA                           
003900*                                 GENERAL FLAG                            
004000     03 MOD-TIREGDAT-9KOMPL-ENTER                                         
004100                             PIC 9(7).                                    
004200*                                 DATUMETS 9-KOMPLEMENT                   
004300*                                 DATES 9-COMPLEMENT                      
004400     03 MOD-TIKLOCK-9KOMPL-ENTER                                          
004500                             PIC 9(9).                                    
004600*                                 TID LAGRAT SOM 9-KOMPLEMENT             
004700*                                 TIME SAVED AS 9-COMPLEMENT              
004800     03 MOD-TIREGDAT-9KOMPL-NEXT                                          
004900                             PIC 9(7).                                    
005000*                                 DATUMETS 9-KOMPLEMENT                   
005100*                                 DATES 9-COMPLEMENT                      
005200     03 MOD-TIKLOCK-9KOMPL-NEXT                                           
005300                             PIC 9(9).                                    
005400*                                 TID LAGRAT SOM 9-KOMPLEMENT             
005500*                                 TIME SAVED AS 9-COMPLEMENT              
005600     03 MOD-LEVEL2-IDDC-NEXT PIC X(2).                                    
005700*                                 IDENTIFIERARE LAGER                     
005800*                                 WAREHOUSE IDENTIFIER                    
005900     03 MOD-LEVEL3-IDDC-NEXT PIC X(2).                                    
006000*                                 IDENTIFIERARE LAGER                     
006100*                                 WAREHOUSE IDENTIFIER                    
006200     03 MOD-RAD              OCCURS 13 TIMES.                             
006300*                                 LINES                                   
006400        05 MOD-IDDC          PIC X(2).                                    
006500*                                 IDENTIFIERARE LAGER                     
006600*                                 WAREHOUSE IDENTIFIER                    
006700        05 MOD-TISTADAT      PIC 9(6).                                    
006800*                                 GENERELLT STARTDATUM                    
006900*                                 GENERAL START DATE                      
007000        05 MOD-TISTODAT      PIC 9(6).                                    
007100*                                 GENERELLT STOPPDATUM                    
007200*                                 GENERAL STOP DATE YYMMDD                
007300        05 MOD-KVANTAL       PIC Z(7).                                    
007400*                                 ANTAL                                   
007500*                                 NUMBER                                  
007600        05 MOD-KVAVV-KVAL    PIC Z(5)9.                                   
007700*                                 ANTALSAVVIKELSE KVALITET                
007800*                                 QUANTITYDEVIATION QUALITY               
007900        05 MOD-KVART-SKROT   PIC Z(5)9.                                   
008000*                                 ANTAL SKROTADE ARTIKLAR                 
008100*                                 QUANTITY INSPECTED PARTS                
008200        05 MOD-KVART-RET     PIC Z(5)9.                                   
008300*                                 ANTAL ARTIKLAR I RETUR                  
008400*                                 QUANTITY INSPECTED PARTS                
008500        05 MOD-KVART-KJUST   PIC Z(5)9.                                   
008600*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
008700*                                 QUANTITY INSPECTED PARTS                
008800        05 MOD-BEINIT        PIC X(3).                                    
008900*                                 INITIALER FÖR EN PERSON                 
009000*                                 INITIALS FOR A PERSON                   
009100        05 MOD-KVLS          PIC Z(5)9.                                   
009200*                                 LAGERSALDO                              
009300*                                 STOCK BALANCE                           
009400        05 MOD-KDLEVSP       PIC Z9.                                      
009500*                                 SPÄRRKOD LEVERANS                       
009600*                                 DELIVERY BLOCKING CODE                  
009700        05 MOD-KVSPARR-KVAL  PIC Z(5)9.                                   
009800*                                 SPÄRRAT ANTAL KVALITETSFEL              
009900*                                 BLOCKED QUANTITY QUALITY ERROR          
010000        05 MOD-FLLEV         PIC X.                                       
010100*                                 ALLMÄN FLAGGA                           
010200*                                 GENERAL FLAG                            
010300        05 MOD-FLFIN         PIC X.                                       
010400*                                 ALLMÄN FLAGGA                           
010500*                                 GENERAL FLAG                            
010600        05 MOD-FL-DC-INFO    PIC X.                                       
010700*                                 ALLMÄN FLAGGA                           
010800*                                 GENERAL FLAG                            
010900     03 MOD-IN-RAD.                                                       
011000*                                 LINES                                   
011100        05 MOD-IDDC-UPPD-ATTR                                             
011200                             PIC X(2).                                    
011300*                                 MFS ATTRIBUTFÄLT                        
011400        05 MOD-IDDC-UPPD     PIC X(2).                                    
011500*                                 IDENTIFIERARE LAGER                     
011600*                                 WAREHOUSE IDENTIFIER                    
011700        05 MOD-TISTADAT-UPPD-ATTR                                         
011800                             PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000        05 MOD-TISTADAT-UPPD PIC 9(6).                                    
012100*                                 GENERELLT STARTDATUM                    
012200*                                 GENERAL START DATE                      
012300        05 MOD-TISTODAT-UPPD-ATTR                                         
012400                             PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600        05 MOD-TISTODAT-UPPD PIC 9(6).                                    
012700*                                 GENERELLT STOPPDATUM                    
012800*                                 GENERAL STOP DATE YYMMDD                
012900        05 MOD-KVANTAL-UPPD-ATTR                                          
013000                             PIC X(2).                                    
013100*                                 MFS ATTRIBUTFÄLT                        
013200        05 MOD-KVANTAL-UPPD  PIC 9(7).                                    
013300*                                 ANTAL                                   
013400*                                 NUMBER                                  
013500        05 MOD-KVAVV-KVAL-UPPD-ATTR                                       
013600                             PIC X(2).                                    
013700*                                 MFS ATTRIBUTFÄLT                        
013800        05 MOD-KVAVV-KVAL-UPPD                                            
013900                             PIC 9(7).                                    
014000*                                 ANTALSAVVIKELSE KVALITET                
014100*                                 QUANTITYDEVIATION QUALITY               
014200        05 MOD-KVART-SKROT-UPPD-ATTR                                      
014300                             PIC X(2).                                    
014400*                                 MFS ATTRIBUTFÄLT                        
014500        05 MOD-KVART-SKROT-UPPD                                           
014600                             PIC 9(7).                                    
014700*                                 ANTAL SKROTADE ARTIKLAR                 
014800*                                 QUANTITY INSPECTED PARTS                
014900        05 MOD-KVART-RET-UPPD-ATTR                                        
015000                             PIC X(2).                                    
015100*                                 MFS ATTRIBUTFÄLT                        
015200        05 MOD-KVART-RET-UPPD                                             
015300                             PIC 9(7).                                    
015400*                                 ANTAL ARTIKLAR I RETUR                  
015500*                                 QUANTITY INSPECTED PARTS                
015600        05 MOD-KVART-KJUST-UPPD-ATTR                                      
015700                             PIC X(2).                                    
015800*                                 MFS ATTRIBUTFÄLT                        
015900        05 MOD-KVART-KJUST-UPPD                                           
016000                             PIC 9(7).                                    
016100*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
016200*                                 QUANTITY INSPECTED PARTS                
016300        05 MOD-BEINIT-UPPD-ATTR                                           
016400                             PIC X(2).                                    
016500*                                 MFS ATTRIBUTFÄLT                        
016600        05 MOD-BEINIT-UPPD   PIC X(3).                                    
016700*                                 INITIALER FÖR EN PERSON                 
016800*                                 INITIALS FOR A PERSON                   
016900     03 MOD-TEMFSINF         PIC X(55).                                   
017000*                                 INFORMATIONSMEDDELANDE                  
017100*                                 INFORMATION MESSAGE                     
017200*** END OF VILMAII-COPY LENGTH= 1090 BYTES                                
