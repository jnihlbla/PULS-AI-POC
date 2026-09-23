000100*** EDIT ALLOWED                                                          
000200*                            *************************************        
000300*                            *** ANVÄNDS VID TEST AV:                     
000400*                            ***  - SDC/LDC DISTRIKT SDC/LDC->CDC         
000500*                            ***  - SDC/LDC DISTRIKT CDC->SDC/LDC         
000600*                            ***  - ST  DISTRIKT ST  -> CDC               
000700*                            ***  - ST  DISTRIKT CDC -> ST                
000800*                            ***  - SDC DISTRIKT SDCNL -> SITTARD         
000900*                            ***  - SDC DISTRIKT FLEN-> SDCNL             
001000*                            ***                                          
001100*                            ***  - NDC DISTRIKT NDC -> CDC               
001200*                            ***  - NDC DISTRIKT NDC -> NDC               
001300*                            ***  - NDC DISTRIKT CDC -> NDC               
001400*                            ***                                          
001500*                            *** OBS!  FINNS ÄVEN PÅ EPLUSCPY MEN         
001600*                            ***       BARA MED REFILL DISTRIKT           
001700*                            ***       GLÖM INTE ÄNDRA DÄR VID            
001800*                            ***       ÄNDRING HÄR.                       
001900*                            ***                                          
002000*                            *** OBS!  VID ÄNDRING AV REFILL              
002100*                            ***       GLÖM INTE ÄNDRA I                  
002200*                            ***       TABELLEN I WWDIST57.               
002300*                            ***                                          
002400*                            *************************************        
002500                                                                          
002600 01  DIST35-IDDISTR          PIC 9(5)     COMP-3.                         
002700                                                                          
002800*- BOTH RETUR AND RETUR-QUAL (ALL NON VCC MARKETS)                        
002900     88  DIST35-RETUR              VALUE  8033 8037 8063                  
003000                                          8083                            
003100                                          8034 8064                       
003200                                          8084                            
003300                                          8780 8790                       
003400                                          8880 8886 8887 8888             
003500                                          8890 8896 8897 8899             
003600                                          8311 8312 8363 8393.            
003700*                                                                         
003800*- ONLY RETUR-QUAL  (NOT NA & CN-NDC:S)                                   
003900     88  DIST35-RETUR-Q            VALUE  8034 8064                       
004000                                          8084                            
004100                                          8312                            
004200                                          8790                            
004300                                          8890 8896 8897 8899.            
004400*                                                                         
004500     88  DIST35-NL-CDC-RETUR       VALUE  8033.                           
004600     88  DIST35-NL-CDC-RETUR-Q     VALUE  8034.                           
004700     88  DIST35-CDC-NL-RETUR       VALUE  8037.                           
004800     88  DIST35-ES-CDC-RETUR       VALUE  8063.                           
004900     88  DIST35-ES-CDC-RETUR-Q     VALUE  8064.                           
005000     88  DIST35-AT-CDC-RETUR       VALUE  8083.                           
005100     88  DIST35-AT-CDC-RETUR-Q     VALUE  8084.                           
005200     88  DIST35-JPAU-CDC-RETUR     VALUE  8311.                           
005300     88  DIST35-JPAU-CDC-RETUR-Q   VALUE  8312.                           
005400*                                                                         
005500*- INCLUDES ALL SWEDISH LDC:S 1*                                          
005600     88  DIST35-SE-LDC-CDC-RETUR   VALUE  8780.                           
005700     88  DIST35-SE-LDC-CDC-RETUR-Q VALUE  8790.                           
005800*                                                                         
005900*- INCLUDES ALL EUROPEAN LDC:S 2* AND 3*                                  
006000     88  DIST35-EU-LDC-CDC-RETUR   VALUE  8880.                           
006100     88  DIST35-EU-LDC-CDC-RETUR-Q VALUE  8890.                           
006200     88  DIST35-EES-LDC-CDC-RETUR   VALUE 8886 8887 8888.                 
006300     88  DIST35-EES-LDC-CDC-RETUR-Q VALUE 8896 8897 8899.                 
006400*                                                                         
006500*- RETURNS FROM NDC'S TO CDC                                              
006600*-  RETURDISTRIKTEN ÄR DETSAMMA FÖR SAMTLIGA NORDAMERIKANSKA              
006700*-  NDC:ER. ETT DISTRIKT FÖR BUYBACK OCH ETT FÖR KVALITET.                
006800*-  DC SKILJER DEM ÅT. ANVÄND DETTA FÖR TESTER                            
006900     88  DIST35-NA-CDC-RETURN     VALUE  8111 8211.                       
007000     88  DIST35-NA-CDC-BB-RETURN   VALUE  8111.                           
007100     88  DIST35-NA-CDC-QUAL-RETURN VALUE  8211.                           
007200*                                                                         
007300*- RETUR AND RETUR-QUAL CHINA                                             
007400     88  DIST35-CN-CDC-RETURNS     VALUE  8200 8201.                      
007500     88  DIST35-CN-CDC-RETUR       VALUE  8200.                           
007600     88  DIST35-CN-CDC-RETUR-Q     VALUE  8201.                           
007700*                                                                         
007800*- RETUR AND RETUR-QUAL INDIA                                             
007900     88  DIST35-IN-CDC-RETURNS     VALUE  8202 8203.                      
008000     88  DIST35-IN-CDC-RETUR       VALUE  8202.                           
008100     88  DIST35-IN-CDC-RETUR-Q     VALUE  8203.                           
008200*                                                                         
008300*- RETUR AND RETUR-QUAL KOREA                                             
008400     88  DIST35-KR-CDC-RETURNS     VALUE  8204 8205.                      
008500     88  DIST35-KR-CDC-RETUR       VALUE  8204.                           
008600     88  DIST35-KR-CDC-RETUR-Q     VALUE  8205.                           
008700*                                                                         
008800*- RETUR AND RETUR-QUAL THAILAND                                          
008900     88  DIST35-TH-CDC-RETURNS     VALUE  8206 8207                       
009000                                          8226 8227.                      
009100     88  DIST35-TH-CDC-RETUR       VALUE  8206.                           
009200     88  DIST35-TH-CDC-RETUR-Q     VALUE  8207.                           
009300     88  DIST35-TH-93-CDC-RETUR    VALUE  8226.                           
009400     88  DIST35-TH-93-CDC-RETUR-Q  VALUE  8227.                           
009500*                                                                         
009600*- RETUR AND RETUR-QUAL TAIWAN                                            
009700     88  DIST35-TW-CDC-RETURNS     VALUE  8208 8209.                      
009800     88  DIST35-TW-CDC-RETUR       VALUE  8208.                           
009900     88  DIST35-TW-CDC-RETUR-Q     VALUE  8209.                           
010000*                                                                         
010100*- RETUR AND RETUR-QUAL MALAYSIA                                          
010200     88  DIST35-MY-CDC-RETURNS     VALUE  8212 8213.                      
010300     88  DIST35-MY-CDC-RETUR       VALUE  8212.                           
010400     88  DIST35-MY-CDC-RETUR-Q     VALUE  8213.                           
010500*                                                                         
010600*- RETUR AND RETUR-QUAL RUSSIA                                            
010700     88  DIST35-RU-CDC-RETURNS     VALUE  8214 8215.                      
010800     88  DIST35-RU-CDC-RETUR       VALUE  8214.                           
010900     88  DIST35-RU-CDC-RETUR-Q     VALUE  8215.                           
011000*                                                                         
011100*- RETUR AND RETUR-QUAL BRASIL                                            
011200     88  DIST35-BR-CDC-RETURNS     VALUE  8216 8217.                      
011300     88  DIST35-BR-CDC-RETUR       VALUE  8216.                           
011400     88  DIST35-BR-CDC-RETUR-Q     VALUE  8217.                           
011500*                                                                         
011600*- RETUR AND RETUR-QUAL MEXICO                                            
011700     88  DIST35-MX-CDC-RETURNS     VALUE  8218 8219.                      
011800     88  DIST35-MX-CDC-RETUR       VALUE  8218.                           
011900     88  DIST35-MX-CDC-RETUR-Q     VALUE  8219.                           
012000*                                                                         
012100*- RETUR AND RETUR-QUAL SOUTH AFRICA                                      
012200     88  DIST35-ZA-CDC-RETURNS     VALUE  8220 8221.                      
012300     88  DIST35-ZA-CDC-RETUR       VALUE  8220.                           
012400     88  DIST35-ZA-CDC-RETUR-Q     VALUE  8221.                           
012500*                                                                         
012600*- RETUR AND RETUR-QUAL TURKEY                                            
012700     88  DIST35-TR-CDC-RETURNS     VALUE  8222 8223.                      
012800     88  DIST35-TR-CDC-RETUR       VALUE  8222.                           
012900     88  DIST35-TR-CDC-RETUR-Q     VALUE  8223.                           
013000*                                                                         
013100*- RETUR AND RETUR-QUAL EMIRATES                                          
013200     88  DIST35-AE-CDC-RETURNS     VALUE  8224 8225.                      
013300     88  DIST35-AE-CDC-RETUR       VALUE  8224.                           
013400     88  DIST35-AE-CDC-RETUR-Q     VALUE  8225.                           
013500*                                                                         
013600*- RETURER INOM KINA TILL NDC:ERNA (OBS ! EJ RETUR-QUAL)                  
013700     88  DIST35-CN-NDC-RETURNS     VALUE  8371 8372 8373 8374.            
013800     88  DIST35-CN-NDC71-RETUR     VALUE  8371.                           
013900     88  DIST35-CN-NDC72-RETUR     VALUE  8372.                           
014000     88  DIST35-CN-NDC73-RETUR     VALUE  8373.                           
014100     88  DIST35-CN-NDC74-RETUR     VALUE  8374.                           
014200*                                                                         
014300*- RETURER INOM NA TILL NDC:ERNA (OBS ! EJ RETUR-QUAL)                    
014400     88  DIST35-NA-NDC-RETURNS     VALUE  8320 THRU 8329                  
014500                                          8721 THRU 8729.                 
014600     88  DIST35-US-CA-RETUR        VALUE  8320.                           
014700     88  DIST35-US-US-RETUR        VALUE  8321 THRU 8329.                 
014800     88  DIST35-CA-US-RETUR        VALUE  8721 THRU 8729.                 
014900     88  DIST35-US-NDC51-RETUR     VALUE  8320.                           
015000     88  DIST35-US-NDC41-RETUR     VALUE  8321.                           
015100     88  DIST35-US-NDC43-RETUR     VALUE  8323.                           
015200     88  DIST35-US-NDC44-RETUR     VALUE  8324.                           
015300     88  DIST35-US-NDC45-RETUR     VALUE  8325.                           
015400     88  DIST35-US-NDC46-RETUR     VALUE  8326.                           
015500     88  DIST35-US-NDC47-RETUR     VALUE  8327.                           
015600     88  DIST35-CA-NDC41-RETUR     VALUE  8721.                           
015700     88  DIST35-CA-NDC43-RETUR     VALUE  8723.                           
015800     88  DIST35-CA-NDC44-RETUR     VALUE  8724.                           
015900     88  DIST35-CA-NDC45-RETUR     VALUE  8725.                           
016000     88  DIST35-CA-NDC46-RETUR     VALUE  8726.                           
016100     88  DIST35-CA-NDC47-RETUR     VALUE  8727.                           
016200*                                                                         
016300*- RETURER INOM JP TILL NDC:ERNA (OBS ! EJ RETUR-QUAL)                    
016400     88  DIST35-JP-NDC-RETURNS     VALUE  8363.                           
016500     88  DIST35-JP-NDC61-RETUR     VALUE  8363.                           
016600*                                                                         
016700*- RETURER INOM TH TILL NDC:ERNA (OBS ! EJ RETUR-QUAL)                    
016800     88  DIST35-TH-NDC-RETURNS     VALUE  8393.                           
016900     88  DIST35-TH-NDC63-RETUR     VALUE  8393.                           
017000*                                                                         
017100*- RETURNS FROM NON-VCC OWNED NDC'S TO CDC                                
017200     88  DIST35-CDC-RETURNS-NON-VCC    VALUE  8200 8201 8202 8203         
017300                                              8204 8205 8206 8207         
017400                                              8208 8209                   
017500                                              8212 THRU 8225.             
017600*                                                                         
017700*                                                                         
017800******************************************************************        
017900* REFILL FROM VCC                                                         
018000******************************************************************        
018100*   REFILL FROM CDC                                                       
018200******************************************************************        
018300*- REFILL (ONLY FROM CDC)                                                 
018400     88  DIST35-REFILL             VALUE  8032 8042 8052 8062             
018500                                          8072 8082 8092                  
018600                                          8141 THRU 8149                  
018700                                          8151 THRU 8153                  
018800                                          8161 THRU 8167                  
018900                                          8171 THRU 8174                  
019000                                          8181 THRU 8187                  
019100                                          8193                            
019200                                          8500                            
019300                                          8700 THRU 8706                  
019400                                          8720                            
019500                                          8800 THRU 8814                  
019600                                          8832                            
019700                                          8851 THRU 8868                  
019800                                          8900 THRU 8907.                 
019900*                                                                         
020000     88  DIST35-CDC-NONVCC-REFILL  VALUE  8152 8153                       
020100                                          8163 THRU 8167                  
020200                                          8171 THRU 8174                  
020300                                          8181 THRU 8187                  
020400                                          8193.                           
020500*                                                                         
020600     88  DIST35-CDC-LDC-REFILL     VALUE  8092                            
020700                                          8700 THRU 8706                  
020800                                          8720                            
020900                                          8800 THRU 8814                  
021000                                          8851 THRU 8868                  
021100                                          8900 THRU 8907.                 
021200*                                                                         
021300     88  DIST35-SE-REFILL          VALUE  8700 THRU 8706 8720.            
021400*                                                                         
021500     88  DIST35-EES-REFILL         VALUE  8092                            
021600                                          8802 8807                       
021700                                          8851 8857 8859.                 
021800*                                                                         
021900*- REFILL PER DC                                                          
022000     88  DIST35-CDC-NL-REFILL      VALUE  8032.                           
022100     88  DIST35-CDC-FR-REFILL      VALUE  8042.                           
022200     88  DIST35-CDC-GB-REFILL      VALUE  8052.                           
022300     88  DIST35-CDC-ES-REFILL      VALUE  8062.                           
022400     88  DIST35-CDC-IT-REFILL      VALUE  8072.                           
022500     88  DIST35-CDC-AT-REFILL      VALUE  8082.                           
022600     88  DIST35-CDC-GB-3A-REFILL   VALUE  8092.                           
022700     88  DIST35-CDC-BR-REFILL      VALUE  8152.                           
022800     88  DIST35-CDC-MX-REFILL      VALUE  8153.                           
022900     88  DIST35-CDC-AU-REFILL      VALUE  8162.                           
023000     88  DIST35-CDC-TH-REFILL      VALUE  8163.                           
023100     88  DIST35-CDC-TW-REFILL      VALUE  8164.                           
023200     88  DIST35-CDC-KR-REFILL      VALUE  8165.                           
023300     88  DIST35-CDC-MY-REFILL      VALUE  8166.                           
023400     88  DIST35-CDC-IN-REFILL      VALUE  8167.                           
023500     88  DIST35-CDC-ZA-REFILL      VALUE  8185.                           
023600     88  DIST35-CDC-TR-REFILL      VALUE  8186.                           
023700     88  DIST35-CDC-AE-REFILL      VALUE  8187.                           
023800     88  DIST35-CDC-TH-93-REFILL   VALUE  8193.                           
023900     88  DIST35-CDC-1A-REFILL      VALUE  8700.                           
024000     88  DIST35-CDC-1B-REFILL      VALUE  8701.                           
024100     88  DIST35-CDC-1C-REFILL      VALUE  8702.                           
024200     88  DIST35-CDC-1D-REFILL      VALUE  8703.                           
024300     88  DIST35-CDC-1E-REFILL      VALUE  8704.                           
024400     88  DIST35-CDC-1F-REFILL      VALUE  8705.                           
024500     88  DIST35-CDC-1G-REFILL      VALUE  8706.                           
024600     88  DIST35-CDC-1K-REFILL      VALUE  8720.                           
024700     88  DIST35-CDC-2A-REFILL      VALUE  8800.                           
024800     88  DIST35-CDC-2B-REFILL      VALUE  8801.                           
024900     88  DIST35-CDC-2C-REFILL      VALUE  8802.                           
025000     88  DIST35-CDC-2D-REFILL      VALUE  8803.                           
025100     88  DIST35-CDC-2E-REFILL      VALUE  8804.                           
025200     88  DIST35-CDC-2F-REFILL      VALUE  8805.                           
025300     88  DIST35-CDC-2G-REFILL      VALUE  8806.                           
025400     88  DIST35-CDC-2H-REFILL      VALUE  8807.                           
025500     88  DIST35-CDC-2I-REFILL      VALUE  8808.                           
025600     88  DIST35-CDC-2J-REFILL      VALUE  8809.                           
025700     88  DIST35-CDC-2K-REFILL      VALUE  8810.                           
025800     88  DIST35-CDC-2L-REFILL      VALUE  8811.                           
025900     88  DIST35-CDC-2M-REFILL      VALUE  8812.                           
026000     88  DIST35-CDC-2N-REFILL      VALUE  8813.                           
026100     88  DIST35-CDC-2O-REFILL      VALUE  8814.                           
026200     88  DIST35-CDC-3B-REFILL      VALUE  8851.                           
026300     88  DIST35-CDC-3C-REFILL      VALUE  8852.                           
026400     88  DIST35-CDC-3D-REFILL      VALUE  8853.                           
026500     88  DIST35-CDC-3E-REFILL      VALUE  8854.                           
026600     88  DIST35-CDC-3F-REFILL      VALUE  8855.                           
026700     88  DIST35-CDC-3G-REFILL      VALUE  8856.                           
026800     88  DIST35-CDC-3H-REFILL      VALUE  8857.                           
026900     88  DIST35-CDC-3I-REFILL      VALUE  8858.                           
027000     88  DIST35-CDC-3J-REFILL      VALUE  8859.                           
027100     88  DIST35-CDC-3K-REFILL      VALUE  8860.                           
027200     88  DIST35-CDC-3L-REFILL      VALUE  8861.                           
027300     88  DIST35-CDC-3M-REFILL      VALUE  8862.                           
027400     88  DIST35-CDC-3N-REFILL      VALUE  8863.                           
027500     88  DIST35-CDC-3O-REFILL      VALUE  8864.                           
027600     88  DIST35-CDC-3P-REFILL      VALUE  8865.                           
027700     88  DIST35-CDC-3R-REFILL      VALUE  8866.                           
027800     88  DIST35-CDC-3S-REFILL      VALUE  8867.                           
027900     88  DIST35-CDC-3T-REFILL      VALUE  8868.                           
028000*                                                                         
028100     88  DIST35-REFILL-JP          VALUE  8161 8500.                      
028200     88  DIST35-CDC-61-REFILL      VALUE  8161.                           
028300     88  DIST35-CDC-6A-REFILL      VALUE  8500.                           
028400*                                                                         
028500     88  DIST35-REFILL-NA          VALUE  8141 THRU 8149                  
028600                                          8151.                           
028700     88  DIST35-CDC-NDC41-REFILL   VALUE  8141.                           
028800     88  DIST35-CDC-NDC43-REFILL   VALUE  8143.                           
028900     88  DIST35-CDC-NDC44-REFILL   VALUE  8144.                           
029000     88  DIST35-CDC-NDC45-REFILL   VALUE  8145.                           
029100     88  DIST35-CDC-NDC46-REFILL   VALUE  8146.                           
029200     88  DIST35-CDC-NDC47-REFILL   VALUE  8147.                           
029300     88  DIST35-CDC-NDC51-REFILL   VALUE  8151.                           
029400*                                                                         
029500     88  DIST35-REFILL-NS          VALUE  8152 8153.                      
029600     88  DIST35-CDC-NDC52-REFILL   VALUE  8152.                           
029700     88  DIST35-CDC-NDC53-REFILL   VALUE  8153.                           
029800*                                                                         
029900     88  DIST35-REFILL-NP          VALUE  8161 THRU 8167                  
030000                                          8193 8500.                      
030100     88  DIST35-CDC-NDC61-REFILL   VALUE  8161.                           
030200     88  DIST35-CDC-NDC62-REFILL   VALUE  8162.                           
030300     88  DIST35-CDC-NDC63-REFILL   VALUE  8163.                           
030400     88  DIST35-CDC-NDC64-REFILL   VALUE  8164.                           
030500     88  DIST35-CDC-NDC65-REFILL   VALUE  8165.                           
030600     88  DIST35-CDC-NDC66-REFILL   VALUE  8166.                           
030700     88  DIST35-CDC-NDC67-REFILL   VALUE  8167.                           
030800     88  DIST35-CDC-NDC93-REFILL   VALUE  8193.                           
030900     88  DIST35-CDC-NDC6A-REFILL   VALUE  8500.                           
031000*                                                                         
031100     88  DIST35-REFILL-CN          VALUE  8171 THRU 8174.                 
031200     88  DIST35-CDC-NDC71-REFILL   VALUE  8171.                           
031300     88  DIST35-CDC-NDC72-REFILL   VALUE  8172.                           
031400     88  DIST35-CDC-NDC73-REFILL   VALUE  8173.                           
031500     88  DIST35-CDC-NDC74-REFILL   VALUE  8174.                           
031600*                                                                         
031700     88  DIST35-REFILL-NX          VALUE  8181 THRU 8187.                 
031800     88  DIST35-CDC-NDC81-REFILL   VALUE  8181.                           
031900     88  DIST35-CDC-NDC82-REFILL   VALUE  8182.                           
032000     88  DIST35-CDC-NDC83-REFILL   VALUE  8183.                           
032100     88  DIST35-CDC-NDC84-REFILL   VALUE  8184.                           
032200     88  DIST35-CDC-NDC85-REFILL   VALUE  8185.                           
032300     88  DIST35-CDC-NDC86-REFILL   VALUE  8186.                           
032400     88  DIST35-CDC-NDC87-REFILL   VALUE  8187.                           
032500*                                                                         
032600*************************************************************             
032700*   REFILL FROM VCC BUT NOT FROM CDC                                      
032800*************************************************************             
032900*- REFILL MELLAN JAPAN NDC:ER                                             
033000     88  DIST35-REFILL-INOM-JP     VALUE  8263.                           
033100     88  DIST35-NDC-NDC6A-REFILL   VALUE  8263.                           
033200*                                                                         
033300*- REFILL MELLAN JAPAN OCH NA NDC:ER                                      
033400     88  DIST35-REFILL-NA-JAP      VALUE  8541 8542 8543                  
033500                                          8544 8551.                      
033600     88  DIST35-JAP-NDC41-REFILL   VALUE  8541.                           
033700     88  DIST35-JAP-NDC43-REFILL   VALUE  8543.                           
033800     88  DIST35-JAP-NDC44-REFILL   VALUE  8544.                           
033900     88  DIST35-JAP-NDC51-REFILL   VALUE  8551.                           
034000*                                                                         
034100* FROM JAPAN OR AUSTRALIA TO NON VCC                                      
034200     88  DIST35-VCC-NONVCC-REFILL VALUE  9465 9467 9471.                  
034300*                                                                         
034400     88  DIST35-JP-NONVCC-REFILL   VALUE  9465 9467 9471.                 
034500*                                                                         
034600     88  DIST35-JP-NDC65-REFILL    VALUE  9465.                           
034700     88  DIST35-JP-NDC67-REFILL    VALUE  9467.                           
034800     88  DIST35-JP-NDC71-REFILL    VALUE  9471.                           
034900*                                                                         
035000*                                                                         
035100*************************************************************             
035200*   REFILL FOR NON VCC WITHIN THE COUNTRY                                 
035300*   REFILL FOR US/CA AS LONG THEY ARE UNDER VCC                           
035400*   REFILL WITHIN JAPAN IN THE DIST35-REFILL-INOM-NDC GROUP               
035500*************************************************************             
035600*- REFILL MELLAN LDC/NDC:ER                                               
035700     88  DIST35-REFILL-INOM-NDC    VALUE  8271 8272 8273 8274             
035800                                          8263 8293                       
035900                                          8950 THRU 8957                  
036000                                          8330 THRU 8339                  
036100                                          8731 THRU 8739.                 
036200*                                                                         
036300*- REFILL MELLAN NONVCC NDC:ER                                            
036400     88  DIST35-REFILL-INOM-NONVCC-NDC                                    
036500                                   VALUE  8271 8272 8273 8274             
036600                                          8293                            
036700                                          8950 THRU 8957.                 
036800*                                                                         
036900*- REFILL MELLAN KINA LDC/NDC:ER                                          
037000     88  DIST35-REFILL-INOM-CN     VALUE  8271 8272 8273 8274             
037100                                          8950 THRU 8957.                 
037200     88  DIST35-NDC-NDC71-REFILL   VALUE  8271.                           
037300     88  DIST35-NDC-NDC72-REFILL   VALUE  8272.                           
037400     88  DIST35-NDC-NDC73-REFILL   VALUE  8273.                           
037500     88  DIST35-NDC-NDC74-REFILL   VALUE  8274.                           
037600*                                                                         
037700*- REFILL MELLAN THAILAND NDC:ER                                          
037800     88  DIST35-REFILL-INOM-TH     VALUE  8293.                           
037900     88  DIST35-NDC-NDC93-REFILL   VALUE  8293.                           
038000*                                                                         
038100*- REFILL MELLAN NA NDC:ER                                                
038200     88  DIST35-REFILL-INOM-NA     VALUE  8330 THRU 8339                  
038300                                          8731 THRU 8739.                 
038400     88  DIST35-USA-CA-REFILL      VALUE  8330.                           
038500     88  DIST35-USA-USA-REFILL     VALUE  8331 THRU 8339.                 
038600     88  DIST35-CA-USA-REFILL      VALUE  8731 THRU 8739.                 
038700     88  DIST35-USA-NDC51-REFILL   VALUE  8330.                           
038800     88  DIST35-NDC-NDC41-REFILL   VALUE  8331.                           
038900     88  DIST35-NDC-NDC43-REFILL   VALUE  8333.                           
039000     88  DIST35-NDC-NDC44-REFILL   VALUE  8334.                           
039100     88  DIST35-NDC-NDC45-REFILL   VALUE  8335.                           
039200     88  DIST35-NDC-NDC46-REFILL   VALUE  8336.                           
039300     88  DIST35-NDC-NDC47-REFILL   VALUE  8337.                           
039400     88  DIST35-CA-NDC41-REFILL    VALUE  8731.                           
039500     88  DIST35-CA-NDC43-REFILL    VALUE  8733.                           
039600     88  DIST35-CA-NDC44-REFILL    VALUE  8734.                           
039700     88  DIST35-CA-NDC45-REFILL    VALUE  8735.                           
039800     88  DIST35-CA-NDC46-REFILL    VALUE  8736.                           
039900     88  DIST35-CA-NDC47-REFILL    VALUE  8737.                           
040000*                                                                         
040100*                                                                         
040200******************************************************************        
040300* REFILL FROM NON VCC                                                     
040400******************************************************************        
040500*- REFILL(EXPORT) FROM NON VCC                                            
040600     88  DIST35-NONVCC-REFILL      VALUE  9111                            
040700                                          9141 THRU 9149                  
041100                                          9153                            
040800                                          9161 9162                       
040900                                          9163 THRU 9167                  
041000                                          9181 THRU 9187                  
041100                                          9193                            
041200                                          9211                            
041300                                          9252 9253                       
041400                                          9261 9262                       
041500                                          9263 THRU 9267                  
041600                                          9271 THRU 9274                  
041700                                          9281 THRU 9287                  
041800                                          9361 9362 9364 9365             
041800                                          9366 9367.                      
041900*                                                                         
042000******************************************************************        
042100*   REFILL FROM NON VCC TO VCC CDC                                        
042200******************************************************************        
042300*- REFILL(EXPORT) WITHOUT BOUNCE FROM NON VCC TO CDC                      
042400     88  DIST35-NONVCC-CDC-REFILL  VALUE  9111 9211.                      
042500*                                                                         
042600     88  DIST35-NDCCN-CDC-REFILL   VALUE  9111.                           
042700     88  DIST35-NDCUS-CDC-REFILL   VALUE  9211.                           
042800*                                                                         
042900*                                                                         
043000******************************************************************        
043100*   REFILL FROM NON VCC TO VCC BUT NOT CDC                                
043200******************************************************************        
043300*- REFILL(EXPORT) WITHOUT BOUNCE FROM NON VCC TO VCC NOT CDC              
043400     88  DIST35-NONVCC-VCC-REFILL VALUE   9161 9162                       
043500                                          9261 9262                       
043600                                          9361 9362.                      
043700*                                                                         
043800     88  DIST35-NDCCN-JP-REFILL    VALUE  9161.                           
043900     88  DIST35-NDCCN-AU-REFILL    VALUE  9162.                           
044000     88  DIST35-NDCUS-JP-REFILL    VALUE  9261.                           
044100     88  DIST35-NDCUS-AU-REFILL    VALUE  9262.                           
044200     88  DIST35-NDCTH-JP-REFILL    VALUE  9361.                           
044300     88  DIST35-NDCTH-AU-REFILL    VALUE  9362.                           
044400*                                                                         
044500******************************************************************        
044600*   REFILL FROM NON VCC TO NON VCC                                        
044700******************************************************************        
044800*- REFILL(EXPORT) WITH BOUNCE (DOUBLE INVOICE) FROM/TO NON VCC            
044900     88  DIST35-NONVCC-NONVCC-REFILL VALUE  9141 THRU 9149                
045200                                          9153                            
045000                                          9163 THRU 9167                  
045100                                          9181 THRU 9187                  
045200                                          9193                            
045300                                          9252 THRU 9253                  
045400                                          9263 THRU 9267                  
045500                                          9271 THRU 9274                  
045600                                          9281 THRU 9287                  
045700                                          9364 9365                       
045700                                          9366 9367.                      
045800*                                                                         
045900*ALL REFILL FROM CN TO OTHER NON VCC                                      
046000     88  DIST35-NDCCN-NONVCC-REFILL VALUE 9141 THRU 9149                  
046300                                          9153                            
046100                                          9163 THRU 9167                  
046200                                          9181 THRU 9187                  
046300                                          9193.                           
046400*                                                                         
046500     88  DIST35-NDCCN-NDCUS-REFILL VALUE  9141 THRU 9149.                 
046600*                                                                         
046700     88  DIST35-NDCCN-NDC41-REFILL VALUE  9141.                           
046800     88  DIST35-NDCCN-NDC43-REFILL VALUE  9143.                           
046900     88  DIST35-NDCCN-NDC44-REFILL VALUE  9144.                           
047000     88  DIST35-NDCCN-NDC45-REFILL VALUE  9145.                           
047100     88  DIST35-NDCCN-NDC46-REFILL VALUE  9146.                           
047200     88  DIST35-NDCCN-NDC47-REFILL VALUE  9147.                           
047300*                                                                         
047600     88  DIST35-NDCCN-NDC53-REFILL VALUE  9153.                           
047400     88  DIST35-NDCCN-NDC63-REFILL VALUE  9163.                           
047500     88  DIST35-NDCCN-NDC64-REFILL VALUE  9164.                           
047600     88  DIST35-NDCCN-NDC65-REFILL VALUE  9165.                           
047700     88  DIST35-NDCCN-NDC66-REFILL VALUE  9166.                           
047800     88  DIST35-NDCCN-NDC67-REFILL VALUE  9167.                           
047900     88  DIST35-NDCCN-NDC81-REFILL VALUE  9181.                           
048000     88  DIST35-NDCCN-NDC82-REFILL VALUE  9182.                           
048100     88  DIST35-NDCCN-NDC83-REFILL VALUE  9183.                           
048200     88  DIST35-NDCCN-NDC84-REFILL VALUE  9184.                           
048300     88  DIST35-NDCCN-NDC85-REFILL VALUE  9185.                           
048400     88  DIST35-NDCCN-NDC86-REFILL VALUE  9186.                           
048500     88  DIST35-NDCCN-NDC87-REFILL VALUE  9187.                           
048600     88  DIST35-NDCCN-NDC93-REFILL VALUE  9193.                           
048700*                                                                         
048800*ALL REFILL FROM US TO OTHER NON VCC                                      
048900     88  DIST35-NDCUS-NONVCC-REFILL VALUE 9252 THRU 9253                  
049000                                          9263 THRU 9267                  
049100                                          9271 THRU 9274                  
049200                                          9281 THRU 9287.                 
049300*                                                                         
049400     88  DIST35-NDCUS-NDCCN-REFILL VALUE  9271 THRU 9274.                 
049500*                                                                         
049600     88  DIST35-NDCUS-NDC71-REFILL VALUE  9271.                           
049700     88  DIST35-NDCUS-NDC72-REFILL VALUE  9272.                           
049800     88  DIST35-NDCUS-NDC73-REFILL VALUE  9273.                           
049900     88  DIST35-NDCUS-NDC74-REFILL VALUE  9274.                           
050000*                                                                         
050100     88  DIST35-NDCUS-NDC52-REFILL VALUE  9252.                           
050200     88  DIST35-NDCUS-NDC53-REFILL VALUE  9253.                           
050300     88  DIST35-NDCUS-NDC63-REFILL VALUE  9263.                           
050400     88  DIST35-NDCUS-NDC64-REFILL VALUE  9264.                           
050500     88  DIST35-NDCUS-NDC65-REFILL VALUE  9265.                           
050600     88  DIST35-NDCUS-NDC66-REFILL VALUE  9266.                           
050700     88  DIST35-NDCUS-NDC67-REFILL VALUE  9267.                           
050800     88  DIST35-NDCUS-NDC81-REFILL VALUE  9281.                           
050900     88  DIST35-NDCUS-NDC82-REFILL VALUE  9282.                           
051000     88  DIST35-NDCUS-NDC83-REFILL VALUE  9283.                           
051100     88  DIST35-NDCUS-NDC84-REFILL VALUE  9284.                           
051200     88  DIST35-NDCUS-NDC85-REFILL VALUE  9285.                           
051300     88  DIST35-NDCUS-NDC86-REFILL VALUE  9286.                           
051400     88  DIST35-NDCUS-NDC87-REFILL VALUE  9287.                           
051500*                                                                         
051600*ALL REFILL FROM KR TO OTHER NON VCC                                      
051700     88  DIST35-NDCKR-NDC-REFILL   VALUE  9999.                           
051800*                                                                         
051900*ALL REFILL FROM IN TO OTHER NON VCC                                      
052000     88  DIST35-NDCIN-NDC-REFILL   VALUE  9999.                           
052100*                                                                         
052200*ALL REFILL FROM MY TO OTHER NON VCC                                      
052300     88  DIST35-NDCMY-NDC-REFILL   VALUE  9999.                           
052400*                                                                         
052500*ALL REFILL FROM TH TO OTHER NON VCC                                      
052600     88  DIST35-NDCTH-NDC-REFILL   VALUE  9364 9365 9366 9367.            
052700*                                                                         
052800     88  DIST35-NDCTH-NDC64-REFILL VALUE  9364.                           
052800     88  DIST35-NDCTH-NDC65-REFILL VALUE  9365.                           
052800     88  DIST35-NDCTH-NDC66-REFILL VALUE  9366.                           
052900     88  DIST35-NDCTH-NDC67-REFILL VALUE  9367.                           
053000*                                                                         
053100*ALL REFILL FROM TW TO OTHER NON VCC                                      
053200     88  DIST35-NDCTW-NDC-REFILL   VALUE  9999.                           
053300*                                                                         
053400*                                                                         
053500******************************************************************        
053600*   REFILL FROM NON VCC TO NON VCC WITH THE SAME COMPANY                  
053700******************************************************************        
053800*   HERE WE WILL HAVE CN TO CN                                            
053900*   HERE WE WILL HAVE US TO US WHEN REMOVED FROM LAB                      
054000*                                                                         
054100************************************************************              
054200* SPECIAL REFILL FOR FINANCE                                              
054300************************************************************              
054400* REFILL FOR HANDLING VAT IN FINACE FOR EUROPE                            
054500* REFILL VAT DIST                                                         
054600     88  DIST35-REFILL-VAT-EU      VALUE  8032 8062 8072 8082             
054700                                          8808 THRU 8812                  
054800                                          8852 THRU 8856                  
054900                                          8858                            
055000                                          8860 THRU 8868.                 
055100*                                                                         
055200     88  DIST35-REFILL-VAT-NON-EU  VALUE  8802 8807 8092 8851             
055300                                          8859.                           
055400* RETUR  VAT DIST                                                         
055500     88  DIST35-RETUR-VAT-EU       VALUE  8033 8037 8063 8083             
055600                                          8880.                           
055700*                                                                         
055800     88  DIST35-RETURQ-VAT-EU      VALUE  8034 8064 8084                  
055900                                          8890.                           
056000     88  DIST35-RETUR-VAT-NON-EU   VALUE  8886 8887.                      
056100*                                                                         
056200     88  DIST35-RETURQ-VAT-NON-EU  VALUE  8896 8897.                      
056300*                                                                         
056400******************************************************************        
056500**** TRANSFER FROM VCC                                                    
056600******************************************************************        
056700*                                                                         
056800******************************************************************        
056900**** TRANSFER FROM VCC (NOT CDC) TO VCC (NOT CDC)                         
057000*    88  DIST35-VCC-VCC-TRANSFER VALUE                                    
057100******************************************************************        
057200*                                                                         
057300******************************************************************        
057400**** TRANSFER FROM VCC (NOT CDC) TO NON VCC                               
057500******************************************************************        
057600*                                                                         
057700     88  DIST35-VCC-NONVCC-TRANSFER VALUE 8666.                           
057800     88  DIST35-VCC-MY-TRANSFER     VALUE 8666.                           
057900*                                                                         
058000******************************************************************        
058100**** TRANSFER FROM NON VCC                                                
058200******************************************************************        
058300*                                                                         
058400******************************************************************        
058500**** TRANSFER FROM NON VCC TO VCC                                         
058600******************************************************************        
058700*                                                                         
058800     88  DIST35-NONVCC-VCC-TRANSFER VALUE 8661 8662.                      
058900     88  DIST35-NONVCC-JP-TRANSFER  VALUE 8661.                           
059000     88  DIST35-NONVCC-AU-TRANSFER  VALUE 8662.                           
059100*                                                                         
059200******************************************************************        
059300**** TRANSFER FROM NON VCC TO NON VCC DIFFERENT COMPANIES                 
059400******************************************************************        
059500*                                                                         
059600     88  DIST35-NONVCC-NONVCC-TRANSFER VALUE 8563 8564 8566.              
059700     88  DIST35-NONVCC-TH-TRANSFER     VALUE 8563.                        
059800     88  DIST35-NONVCC-TW-TRANSFER     VALUE 8564.                        
059900     88  DIST35-NONVCC-MY-TRANSFER     VALUE 8566.                        
060000*                                                                         
060100******************************************************************        
060200**** TRANSFER FROM NON VCC TO NON VCC (WITHIN SAME COMPANY)               
060300******************************************************************        
060400*- TRANSFERDISTRIKEN (REFILL MELLAN NDC:ERNA) SKALL ANVÄNDAS              
060500*- I KOMBINATION MED DC FÖR ATT SÄRSKILJA DE OLIKA NDC:ERNA.              
060600     88  DIST35-NA-TRANSFER          VALUE  8341 THRU 8349                
060700                                            8392                          
060800                                            8741 THRU 8749                
060900                                            8751 8792.                    
061000     88  DIST35-US-US-TRANSFER       VALUE  8341 THRU 8349                
061100                                            8392.                         
061200     88  DIST35-CAN-US-TRANSFER      VALUE  8741 THRU 8749                
061300                                            8792.                         
061400     88  DIST35-US-CAN-TRANSFER      VALUE  8751.                         
061500     88  DIST35-FROM-US-TO-NDC41     VALUE  8341.                         
061600     88  DIST35-FROM-US-TO-NDC43     VALUE  8343.                         
061700     88  DIST35-FROM-US-TO-NDC44     VALUE  8344.                         
061800     88  DIST35-FROM-US-TO-NDC45     VALUE  8345.                         
061900     88  DIST35-FROM-US-TO-NDC46     VALUE  8346.                         
062000     88  DIST35-FROM-US-TO-NDC47     VALUE  8347.                         
062100     88  DIST35-FROM-US-TO-NDC92     VALUE  8392.                         
062200     88  DIST35-FROM-NDC51-TO-NDC41  VALUE  8741.                         
062300     88  DIST35-FROM-NDC51-TO-NDC43  VALUE  8743.                         
062400     88  DIST35-FROM-NDC51-TO-NDC44  VALUE  8744.                         
062500     88  DIST35-FROM-NDC51-TO-NDC45  VALUE  8745.                         
062600     88  DIST35-FROM-NDC51-TO-NDC46  VALUE  8746.                         
062700     88  DIST35-FROM-NDC51-TO-NDC47  VALUE  8747.                         
062800     88  DIST35-FROM-NDC51-TO-NDC92  VALUE  8792.                         
062900*                                                                         
063000     88  DIST35-PACIFIC-TRANSFER     VALUE  8361 8362.                    
063100     88  DIST35-FROM-AU-TO-JP        VALUE  8361.                         
063200     88  DIST35-FROM-JP-TO-AU        VALUE  8362.                         
063300*                                                                         
063400*- TRANSFER MELLAN KINA LDC/NDC:ER                                        
063500     88  DIST35-CN-TRANSFER          VALUE  8571 8572 8573                
063600                                            8574                          
063700                                            8771 8772 8773                
063800                                            8774 8775 8776                
063900                                            8777 8778                     
064000                                            8871 8872 8873                
064100                                            8874.                         
064200     88  DIST35-FROM-CN-NDC-TO-NDC71 VALUE  8571 8871.                    
064300     88  DIST35-FROM-CN-NDC-TO-NDC72 VALUE  8572 8872.                    
064400     88  DIST35-FROM-CN-NDC-TO-NDC73 VALUE  8573 8873.                    
064500     88  DIST35-FROM-CN-NDC-TO-NDC74 VALUE  8574 8874.                    
064600     88  DIST35-FROM-CN-LDC-TO-NDC71 VALUE  8771 8775.                    
064700     88  DIST35-FROM-CN-LDC-TO-NDC72 VALUE  8772 8776.                    
064800     88  DIST35-FROM-CN-LDC-TO-NDC73 VALUE  8773 8777.                    
064900     88  DIST35-FROM-CN-LDC-TO-NDC74 VALUE  8774 8778.                    
065000*                                                                         
065100*                                                                         
065200     88  DIST35-ST-CDC               VALUE  8011.                         
065300*                                                                         
065400     88  DIST35-NL-SITTARD-OBJEKT    VALUE  9927.                         
065500*                                                                         
065600*** END COPY WWDIST35    LENGTH=3                                         
