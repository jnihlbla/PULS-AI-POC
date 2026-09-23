000100*** EDIT ALLOWED                                                          
010000*                            *************************************        
020027*                            ***INNEHÅLLER ALLA DE KODER SOM              
021008*                            ***ÄR TILLÅTNA ATT ANGE SOM SVAR TILL        
030008*                            ***ÅF:ÅN                                     
060000*                            *************************************        
061008*                            *************************************        
062008*                            ***INNEHÅLLER ALLA DE KODER SOM              
063008*                            ***SKALL SPÄRRA OBJEKT FRÅN                  
064008*                            ***ATT PÅVERKA VÅR BYTESAFFÄR                
065008*                            ***BYTES AFFÄR. TEX PIRATER ELLER            
066008*                            ***SAKNADE OBJEKT ETC                        
066112*                            ***                                          
066216*                            *** BESKRIVNING AV KODER                     
066316*                            ***                                          
066416*                            *** 010 DEMONTERAD, EJ GODKÄND               
066516*                            *** 019 OBJEKT SAKNAS                        
066630*                            *** 029 OBJEKT SAKNAS, EJ MOTTAGNA           
066730*                            ***     INOM SEX MÅN, SKER MASKINELLT        
066830*                            *** 021 ARTIKEL SKADAD                       
066930*                            *** 030 PIRAT - EJ VOLVO                     
067030*                            *** 032 EJ I VOLVOS BYTESSYSTEM              
067130*                            *** 035 OTHER RESON                          
067233*                            *** 040 REJECTED BY TMA                      
067333*                            *** 041 INCORRECT PART                       
067433*                            *** 100 KAMAXEL                              
067533*                            *** 110 INSPRUTNINGSPUMP                     
067633*                            *** 120 RADIO KNOBS MSG                      
067733*                            *** 121 RADIO FACE PLATE                     
067833*                            *** 122 WIRES CUT/AUDIO                      
067933*                            *** 130 TIE RODS MISSING                     
068033*                            *** 115 CYLINDERHUVUD                        
068233*                            *** 170 BROMSOK BYGEL                        
068333*                            *** 200 GARANTI BEHÅLLS I HUSET              
068433*                            *** 210 GARANTI SKICKAS TILL TMA             
068533*                            *** 220 GARANTI UTAN SALDO PÅVERKAN          
068534*                       *** 300 CORE RECEIVED WITHOUT STOCK UPDATE        
068633*                            *** 500 MOTOR VÄXELLÅDA MED OLJA I           
      *                            *** 510 DAMAGED PARTS                        
      *                            *** 511 MISSING PARTS                        
068736*                            *** 910 VÄXELLÅDA EJ TÖMD, SAKNAR            
068836*                            ***     LASTPALL                             
068936*                            *** 920 MOTOR EJ TÖMD, SAKNAR                
069036*                            ***     LASTPALL                             
069136*                            *** 930 VÄXELLÅDA EMBALLAGE                  
069236*                            *** 940 MOTOR EMBALLAGE                      
069236*                            *** 950 INFO STUCK CD/DVD                    
069336*                            *** 960 TESTRAPPORT PÅ AUTVÄXELLÅDA          
069436*                            *** 971 RADIO BOX MISSING (MSG)              
069536*                            ***                                          
069636*                            *** 600 LOKAL USA-XTRA RAD                   
069736*                            *** 601 LOKAL USA-XTRA RAD                   
069836*                            *** 040 LOKAL USA - DELETE LINE              
069936*                            *** 041 LOKAL USE - DELETE LINE              
070036*                            ***                                          
070136*                            ***                                          
070236*                            ***OBS ! KOD '032' KAN ENDAST SÄTTAS         
070336*                            ***AV PGM W30172 DET SKER AUTOMATISKT        
070436*                            ***NÄR ETT ARTIKELNR SAKNAS I VÅRAT          
070536*                            ***REGISTER                                  
070636*                            *************************************        
071034 01  BYT15-KDBYTREF          PIC X(3).                                    
080000*                                                                         
090009     88  BYT15-KDBYTREF-OK     VALUE  '100' '110' '115' '119'             
090132                                      '120' '121' '122' '130'             
090232                                      '161' '162' '200' '210'             
090332                                      '400' '410' '420'                   
090332                                      '430' '440' '450'                   
090332                                      '460' '470' '480' '490'             
090332                                      '500' '501' '502' '503'             
                                            '504' '505' '506' '507'             
                                            '508' '510' '511' '600'             
090332                                      '601' '700' '720' '740'             
090436                                      '910' '920' '930'                   
090535                                      '940' '950' '960' '971'             
090535                                      '972'.                              
091008*                                                                         
100026     88  BYT15-KDBYTREF-REMOVE VALUE  '010' '019' '021'                   
101230                                      '029' '030' '032'                   
101333                                      '035' '040' '041'                   
101334                                      '045' '050' '060'                   
101334                                      '070'.                              
104011*                                                                         
105029     88  BYT15-KDBYTREF-GAR-SALD    VALUE '200' '210'.                    
105119*                                                                         
106029     88  BYT15-KDBYTREF-GAR-EJ-SALD VALUE '220' '300'.                    
110005*** END COPY WWBYT15     LENGTH=0     OLD LENGTH=0                        
