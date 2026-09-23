000100 01  MOD-W6O21201.                                                        
000200*                                 COPYTEXT FÖR MOD W6021201               
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
003400     03 MOD-FLNYSEG-ATTR     PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-FLNYSEG          PIC X.                                       
003700*                                 NYTT SEGMENT                            
003800*                                 NEW SEGMENT                             
003900     03 MOD-TIREGDAT-9KOMPL-ENTER                                         
004000                             PIC 9(7).                                    
004100*                                 DATUMETS 9-KOMPLEMENT                   
004200*                                 DATES 9-COMPLEMENT                      
004300     03 MOD-TIKLOCK-9KOMPL-ENTER                                          
004400                             PIC 9(9).                                    
004500*                                 TID LAGRAT SOM 9-KOMPLEMENT             
004600*                                 TIME SAVED AS 9-COMPLEMENT              
004700     03 MOD-TIREGDAT-9KOMPL-NEXT                                          
004800                             PIC 9(7).                                    
004900*                                 DATUMETS 9-KOMPLEMENT                   
005000*                                 DATES 9-COMPLEMENT                      
005100     03 MOD-TIKLOCK-9KOMPL-NEXT                                           
005200                             PIC 9(9).                                    
005300*                                 TID LAGRAT SOM 9-KOMPLEMENT             
005400*                                 TIME SAVED AS 9-COMPLEMENT              
005500     03 MOD-IDKVAINF-VIEW    PIC X(2).                                    
005600*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
005700*                                 LINENO FOR QUALITY CONTROL TEXT         
005800     03 MOD-TIREGDAT         PIC 9(6).                                    
005900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006000*                                 REGISTRATION DATE (YYMMDD)              
006100     03 MOD-KDPERSON-ATTR    PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-KDPERSON         PIC Z(2)9.                                   
006400*                                 PERSONKOD                               
006500*                                 STAFF CODE                              
006600     03 MOD-BEINIT           PIC X(3).                                    
006700*                                 INITIALER FÖR EN PERSON                 
006800*                                 INITIALS FOR A PERSON                   
006900     03 MOD-TEKVAINP-ATTR    PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100     03 MOD-TEKVAINP         PIC X(20).                                   
007200*                                 KVALITETS INFORMATION ARTIKEL-I         
007300*                                 NPUT                                    
007400*                                 QUALITY INFORMATION PART-INPUT          
007500     03 MOD-TIKLAR-QUAL-ATTR PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 MOD-TIKLAR-QUAL      PIC 9(6).                                    
007800*                                 KLARDATUM          (ÅÅMMDD)             
007900*                                 READY DATE        (YYMMDD)              
008000     03 MOD-FLTABORT-ATTR    PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 MOD-FLTABORT         PIC X.                                       
008300*                                 BORTTAGSFLAGGA                          
008400*                                 DELETE FLAG                             
008500     03 MOD-INT-RADER.                                                    
008600        05 MOD-TEKVAINF-INT-ATTR1                                         
008700                             PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900        05 MOD-TEKVAINF-INT-RAD1                                          
009000                             PIC X(63).                                   
009100*                                 KVALITETS INFORMATION INTERNT           
009200*                                 QUALITY INFORMATION PART NUMBER         
009300*                                  EXTERNAL                               
009400        05 MOD-TEKVAINF-INT-ATTR2                                         
009500                             PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700        05 MOD-TEKVAINF-INT-RAD2                                          
009800                             PIC X(79).                                   
009900*                                 KVALITETS INFORMATION INTERNT           
010000*                                 QUALITY INFORMATION PART NUMBER         
010100*                                  EXTERNAL                               
010200        05 MOD-TEKVAINF-INT-ATTR3                                         
010300                             PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500        05 MOD-TEKVAINF-INT-RAD3                                          
010600                             PIC X(79).                                   
010700*                                 KVALITETS INFORMATION INTERNT           
010800*                                 QUALITY INFORMATION PART NUMBER         
010900*                                  EXTERNAL                               
011000        05 MOD-TEKVAINF-INT-ATTR4                                         
011100                             PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300        05 MOD-TEKVAINF-INT-RAD4                                          
011400                             PIC X(79).                                   
011500*                                 KVALITETS INFORMATION INTERNT           
011600*                                 QUALITY INFORMATION PART NUMBER         
011700*                                  EXTERNAL                               
011800        05 MOD-TEKVAINF-INT-ATTR5                                         
011900                             PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100        05 MOD-TEKVAINF-INT-RAD5                                          
012200                             PIC X(79).                                   
012300*                                 KVALITETS INFORMATION INTERNT           
012400*                                 QUALITY INFORMATION PART NUMBER         
012500*                                  EXTERNAL                               
012600        05 MOD-TEKVAINF-INT-ATTR6                                         
012700                             PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900        05 MOD-TEKVAINF-INT-RAD6                                          
013000                             PIC X(79).                                   
013100*                                 KVALITETS INFORMATION INTERNT           
013200*                                 QUALITY INFORMATION PART NUMBER         
013300*                                  EXTERNAL                               
013400        05 MOD-TEKVAINF-INT-ATTR7                                         
013500                             PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700        05 MOD-TEKVAINF-INT-RAD7                                          
013800                             PIC X(79).                                   
013900*                                 KVALITETS INFORMATION INTERNT           
014000*                                 QUALITY INFORMATION PART NUMBER         
014100*                                  EXTERNAL                               
014200     03 MOD-KDKVAINF-ATTR    PIC X(2).                                    
014300*                                 MFS ATTRIBUTFÄLT                        
014400     03 MOD-KDKVAINF         PIC X.                                       
014500*                                 TYP AV KVAL.INFO FÖR ARTIKEL            
014600*                                 TYPE OF QUAL.INFO. FOR PART             
014700     03 MOD-TIKLAR-LEV-ATTR  PIC X(2).                                    
014800*                                 MFS ATTRIBUTFÄLT                        
014900     03 MOD-TIKLAR-LEV       PIC 9(6).                                    
015000*                                 KLARDATUM          (ÅÅMMDD)             
015100*                                 READY DATE        (YYMMDD)              
015200     03 MOD-FLSTOCH-ATTR     PIC X(2).                                    
015300*                                 MFS ATTRIBUTFÄLT                        
015400     03 MOD-FLSTOCH          PIC X.                                       
015500*                                 STOCKCHECK FLAGGA                       
015600*                                                                         
015700*                                 STOCKCHECK FLAG                         
015800*                                                                         
015900     03 MOD-IDLEVNR          PIC X(5).                                    
016000*                                 LEVERANTÖRNUMMER                        
016100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
016200     03 MOD-FLQPA-ATTR       PIC X(2).                                    
016300*                                 MFS ATTRIBUTFÄLT                        
016400     03 MOD-FLQPA            PIC X.                                       
016500*                                 QUALITY POINT ASSURED FLAGGA            
016600*                                                                         
016700*                                 QUALITY POINT ASSURED FLAG              
016800*                                                                         
016900     03 MOD-EXT-RADER.                                                    
017000        05 MOD-TEKVAINF-EXT-ATTR1                                         
017100                             PIC X(2).                                    
017200*                                 MFS ATTRIBUTFÄLT                        
017300        05 MOD-TEKVAINF-EXT-RAD1                                          
017400                             PIC X(63).                                   
017500*                                 KVALITETS INFORMATION EXTERNT           
017600*                                 QUALITY INFORMATION PART NUMBER         
017700*                                  EXTERNAL                               
017800        05 MOD-TEKVAINF-EXT-ATTR2                                         
017900                             PIC X(2).                                    
018000*                                 MFS ATTRIBUTFÄLT                        
018100        05 MOD-TEKVAINF-EXT-RAD2                                          
018200                             PIC X(79).                                   
018300*                                 KVALITETS INFORMATION EXTERNT           
018400*                                 QUALITY INFORMATION PART NUMBER         
018500*                                  EXTERNAL                               
018600        05 MOD-TEKVAINF-EXT-ATTR3                                         
018700                             PIC X(2).                                    
018800*                                 MFS ATTRIBUTFÄLT                        
018900        05 MOD-TEKVAINF-EXT-RAD3                                          
019000                             PIC X(79).                                   
019100*                                 KVALITETS INFORMATION EXTERNT           
019200*                                 QUALITY INFORMATION PART NUMBER         
019300*                                  EXTERNAL                               
019400        05 MOD-TEKVAINF-EXT-ATTR4                                         
019500                             PIC X(2).                                    
019600*                                 MFS ATTRIBUTFÄLT                        
019700        05 MOD-TEKVAINF-EXT-RAD4                                          
019800                             PIC X(79).                                   
019900*                                 KVALITETS INFORMATION EXTERNT           
020000*                                 QUALITY INFORMATION PART NUMBER         
020100*                                  EXTERNAL                               
020200        05 MOD-TEKVAINF-EXT-ATTR5                                         
020300                             PIC X(2).                                    
020400*                                 MFS ATTRIBUTFÄLT                        
020500        05 MOD-TEKVAINF-EXT-RAD5                                          
020600                             PIC X(79).                                   
020700*                                 KVALITETS INFORMATION EXTERNT           
020800*                                 QUALITY INFORMATION PART NUMBER         
020900*                                  EXTERNAL                               
021000        05 MOD-TEKVAINF-EXT-ATTR6                                         
021100                             PIC X(2).                                    
021200*                                 MFS ATTRIBUTFÄLT                        
021300        05 MOD-TEKVAINF-EXT-RAD6                                          
021400                             PIC X(79).                                   
021500*                                 KVALITETS INFORMATION EXTERNT           
021600*                                 QUALITY INFORMATION PART NUMBER         
021700*                                  EXTERNAL                               
021800        05 MOD-TEKVAINF-EXT-ATTR7                                         
021900                             PIC X(2).                                    
022000*                                 MFS ATTRIBUTFÄLT                        
022100        05 MOD-TEKVAINF-EXT-RAD7                                          
022200                             PIC X(79).                                   
022300*                                 KVALITETS INFORMATION EXTERNT           
022400*                                 QUALITY INFORMATION PART NUMBER         
022500*                                  EXTERNAL                               
022600     03 MOD-TEMFSINF         PIC X(55).                                   
022700*                                 INFORMATIONSMEDDELANDE                  
022800*                                 INFORMATION MESSAGE                     
022900*** END OF VILMAII-COPY LENGTH= 1351 BYTES                                
