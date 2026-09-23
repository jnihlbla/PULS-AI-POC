000100 01  MOD-W6O21401.                                                        
000200*                                 COPYTEXT FÖR MOD W6021401               
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
001600     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001900     03 MOD-IDLEVNR-UT       PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002200     03 MOD-IDKVAINF-IN      PIC X(2).                                    
002300*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
002400*                                 LINENO FOR QUALITY CONTROL TEXT         
002500     03 MOD-IDKVAINF-UT      PIC X(2).                                    
002600*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
002700*                                 LINENO FOR QUALITY CONTROL TEXT         
002800     03 MOD-TIREGDAT-IN      PIC X(6).                                    
002900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003000*                                 REGISTRATION DATE (YYMMDD)              
003100     03 MOD-TIREGDAT-UT      PIC X(6).                                    
003200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003300*                                 REGISTRATION DATE (YYMMDD)              
003400     03 MOD-IDDC-IN          PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600*                                 WAREHOUSE IDENTIFIER                    
003700     03 MOD-IDDC-UT          PIC X(2).                                    
003800*                                 IDENTIFIERARE LAGER                     
003900*                                 WAREHOUSE IDENTIFIER                    
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
005600     03 MOD-IDKVAINF-UPPD-ATTR                                            
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-IDKVAINF-UPPD    PIC X(2).                                    
006000*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
006100*                                 LINENO FOR QUALITY CONTROL TEXT         
006200     03 MOD-IDNAMN           PIC X(26).                                   
006300*                                 NAMN                                    
006400     03 MOD-KDKVAINF         PIC X.                                       
006500*                                 TYP AV KVAL.INFO FÖR ARTIKEL            
006600*                                 TYPE OF QUAL.INFO. FOR PART             
006700     03 MOD-TIKLAR-LEV       PIC 9(6).                                    
006800*                                 KLARDATUM          (ÅÅMMDD)             
006900*                                 READY DATE        (YYMMDD)              
007000     03 MOD-FLSTOCH          PIC X.                                       
007100*                                 STOCKCHECK FLAGGA                       
007200*                                                                         
007300*                                 STOCKCHECK FLAG                         
007400*                                                                         
007500     03 MOD-TIREGDAT         PIC 9(6).                                    
007600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007700*                                 REGISTRATION DATE (YYMMDD)              
007800     03 MOD-IDMAIL           PIC X(60).                                   
007900*                                 MAIL ADRESS                             
008000*                                 MAIL ADDRESS                            
008100     03 MOD-TEKVAINF         OCCURS 7 TIMES                               
008200                             PIC X(79).                                   
008300*                                 KVALITETS INFORMATION                   
008400*                                 QUALITY INFORMATION PART NUMBER         
008500     03 MOD-TISTADAT-UT      PIC 9(6).                                    
008600*                                 GENERELLT STARTDATUM                    
008700*                                 GENERAL START DATE                      
008800     03 MOD-TISTADAT-IN-ATTR PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-TISTADAT-IN      PIC 9(6).                                    
009100*                                 GENERELLT STARTDATUM                    
009200*                                 GENERAL START DATE                      
009300     03 MOD-TISTODAT-UT      PIC 9(6).                                    
009400*                                 GENERELLT STOPPDATUM                    
009500*                                 GENERAL STOP DATE YYMMDD                
009600     03 MOD-TISTODAT-IN-ATTR PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800     03 MOD-TISTODAT-IN      PIC 9(6).                                    
009900*                                 GENERELLT STOPPDATUM                    
010000*                                 GENERAL STOP DATE YYMMDD                
010100     03 MOD-KVANTAL-UT       PIC Z(6)9.                                   
010200*                                 ANTAL                                   
010300*                                 NUMBER                                  
010400     03 MOD-KVANTAL-IN-ATTR  PIC X(2).                                    
010500*                                 MFS ATTRIBUTFÄLT                        
010600     03 MOD-KVANTAL-IN       PIC 9(7).                                    
010700*                                 ANTAL                                   
010800*                                 NUMBER                                  
010900     03 MOD-KVAVV-KVAL-UT    PIC Z(6)9.                                   
011000*                                 ANTALSAVVIKELSE KVALITET                
011100*                                 QUANTITYDEVIATION QUALITY               
011200     03 MOD-KVAVV-KVAL-IN-ATTR                                            
011300                             PIC X(2).                                    
011400*                                 MFS ATTRIBUTFÄLT                        
011500     03 MOD-KVAVV-KVAL-IN    PIC 9(7).                                    
011600*                                 ANTALSAVVIKELSE KVALITET                
011700*                                 QUANTITYDEVIATION QUALITY               
011800     03 MOD-KVART-SKROT-UT   PIC Z(6)9.                                   
011900*                                 ANTAL SKROTADE ARTIKLAR                 
012000*                                 QUANTITY INSPECTED PARTS                
012100     03 MOD-KVART-SKROT-IN-ATTR                                           
012200                             PIC X(2).                                    
012300*                                 MFS ATTRIBUTFÄLT                        
012400     03 MOD-KVART-SKROT-IN   PIC 9(7).                                    
012500*                                 ANTAL SKROTADE ARTIKLAR                 
012600*                                 QUANTITY INSPECTED PARTS                
012700     03 MOD-KVART-RET-UT     PIC Z(6)9.                                   
012800*                                 ANTAL ARTIKLAR I RETUR                  
012900*                                 QUANTITY INSPECTED PARTS                
013000     03 MOD-KVART-RET-IN-ATTR                                             
013100                             PIC X(2).                                    
013200*                                 MFS ATTRIBUTFÄLT                        
013300     03 MOD-KVART-RET-IN     PIC 9(7).                                    
013400*                                 ANTAL ARTIKLAR I RETUR                  
013500*                                 QUANTITY INSPECTED PARTS                
013600     03 MOD-KVART-KJUST-UT   PIC Z(6)9.                                   
013700*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
013800*                                 QUANTITY INSPECTED PARTS                
013900     03 MOD-KVART-KJUST-IN-ATTR                                           
014000                             PIC X(2).                                    
014100*                                 MFS ATTRIBUTFÄLT                        
014200     03 MOD-KVART-KJUST-IN   PIC 9(7).                                    
014300*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
014400*                                 QUANTITY INSPECTED PARTS                
014500     03 MOD-BEINIT-ATTR      PIC X(2).                                    
014600*                                 MFS ATTRIBUTFÄLT                        
014700     03 MOD-BEINIT           PIC X(3).                                    
014800*                                 INITIALER FÖR EN PERSON                 
014900*                                 INITIALS FOR A PERSON                   
015000     03 MOD-IDNAMN-DC-QUAL-ATTR                                           
015100                             PIC X(2).                                    
015200*                                 MFS ATTRIBUTFÄLT                        
015300     03 MOD-IDNAMN-DC-QUAL   PIC X(21).                                   
015400*                                 NAMN                                    
015500     03 MOD-TEKVAINF-DC-RAD1-ATTR                                         
015600                             PIC X(2).                                    
015700*                                 MFS ATTRIBUTFÄLT                        
015800     03 MOD-TEKVAINF-DC-RAD1 PIC X(63).                                   
015900*                                 KVALITETS INFORMATION                   
016000*                                 QUALITY INFORMATION PART NUMBER         
016100     03 MOD-TEKVAINF-DC-RAD2-ATTR                                         
016200                             PIC X(2).                                    
016300*                                 MFS ATTRIBUTFÄLT                        
016400     03 MOD-TEKVAINF-DC-RAD2 PIC X(79).                                   
016500*                                 KVALITETS INFORMATION                   
016600*                                 QUALITY INFORMATION PART NUMBER         
016700     03 MOD-TEKVAINF-DC-RAD3-ATTR                                         
016800                             PIC X(2).                                    
016900*                                 MFS ATTRIBUTFÄLT                        
017000     03 MOD-TEKVAINF-DC-RAD3 PIC X(79).                                   
017100*                                 KVALITETS INFORMATION                   
017200*                                 QUALITY INFORMATION PART NUMBER         
017300     03 MOD-TEKVAINF-DC-RAD4-ATTR                                         
017400                             PIC X(2).                                    
017500*                                 MFS ATTRIBUTFÄLT                        
017600     03 MOD-TEKVAINF-DC-RAD4 PIC X(63).                                   
017700*                                 KVALITETS INFORMATION                   
017800*                                 QUALITY INFORMATION PART NUMBER         
017900     03 MOD-FLTABORT-ATTR    PIC X(2).                                    
018000*                                 MFS ATTRIBUTFÄLT                        
018100     03 MOD-FLTABORT         PIC X.                                       
018200*                                 BORTTAGSFLAGGA                          
018300*                                 DELETE FLAG                             
018400     03 MOD-TEMFSINF         PIC X(55).                                   
018500*                                 INFORMATIONSMEDDELANDE                  
018600*                                 INFORMATION MESSAGE                     
018700*** END OF VILMAII-COPY LENGTH= 1267 BYTES                                
