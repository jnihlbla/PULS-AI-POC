000100*** EDIT ALLOWED                                                          
010000*                            *************************************        
020000*                            *** ANVÄNDS VID TEST AV:                     
030000*                            ***  - FÖRETAGSKOD                           
040000*                            ***  - SAMT KONSTANTER FÖR MOVE              
060000*                            *************************************        
070000*                                                                         
080000 01  WS-IDFTG              PIC 9(2).                                      
090000                                                                          
100000     88  IDFTG-GODKAEND    VALUE  5 6 7 9 53 54 57                        
100100                                  60 61 63 THRU 66                        
100200                                  75 76 81 THRU 87 90.                    
110000                                                                          
120000     88  IDFTG-US          VALUE  53.                                     
150000                                                                          
151000     88  IDFTG-CA          VALUE  54.                                     
152000                                                                          
153000     88  IDFTG-PV          VALUE  5 6 7 9 57 75 76 90.                    
154000                                                                          
155000     88  IDFTG-CN          VALUE  60.                                     
155100                                                                          
155200     88  IDFTG-IN          VALUE  61.                                     
155300                                                                          
155400     88  IDFTG-TH          VALUE  63.                                     
155500                                                                          
155600     88  IDFTG-TW          VALUE  64.                                     
155700                                                                          
155800     88  IDFTG-KR          VALUE  65.                                     
155900                                                                          
155910     88  IDFTG-MY          VALUE  66.                                     
155920                                                                          
155930     88  IDFTG-RU          VALUE  81.                                     
155940                                                                          
155930     88  IDFTG-BR          VALUE  82.                                     
155940                                                                          
155930     88  IDFTG-MX          VALUE  83.                                     
155940                                                                          
155930     88  IDFTG-ZA          VALUE  85.                                     
155940                                                                          
155930     88  IDFTG-TR          VALUE  86.                                     
155940                                                                          
155941     88  IDFTG-AE          VALUE  87.                                     
155942                                                                          
155950     88  IDFTG-NON-VCC     VALUE 60 61 63 64 65 66                        
155950                                 81 THRU 87.                              
156000*                                                                         
156100*                                                                         
156200* KONSTANTER ATT ANVÄNDA ISTÄLLET FÖR HÅRKODNING.                         
156300*                                                                         
157000 01  CONST-IDFTG.                                                         
158000*                                                                         
159000     03  WC-IDFTG-US       PIC 9(2)   VALUE 53.                           
160000*                                                                         
161000     03  WC-IDFTG-CA       PIC 9(2)   VALUE 54.                           
162000*                                                                         
163000     03  WC-IDFTG-PV       PIC 9(2)   VALUE 57.                           
164000*                                                                         
165000     03  WC-IDFTG-CN       PIC 9(2)   VALUE 60.                           
165100*                                                                         
165200     03  WC-IDFTG-IN       PIC 9(2)   VALUE 61.                           
168000*                                                                         
168100     03  WC-IDFTG-TH       PIC 9(2)   VALUE 63.                           
168200*                                                                         
168300     03  WC-IDFTG-TW       PIC 9(2)   VALUE 64.                           
168400*                                                                         
168500     03  WC-IDFTG-KR       PIC 9(2)   VALUE 65.                           
168600*                                                                         
168700     03  WC-IDFTG-MY       PIC 9(2)   VALUE 66.                           
168800*                                                                         
168900     03  WC-IDFTG-RU       PIC 9(2)   VALUE 81.                           
169000*                                                                         
168900     03  WC-IDFTG-BR       PIC 9(2)   VALUE 82.                           
169000*                                                                         
168900     03  WC-IDFTG-MX       PIC 9(2)   VALUE 83.                           
169000*                                                                         
168900     03  WC-IDFTG-ZA       PIC 9(2)   VALUE 85.                           
169000*                                                                         
168900     03  WC-IDFTG-TR       PIC 9(2)   VALUE 86.                           
169000*                                                                         
169100     03  WC-IDFTG-AE       PIC 9(2)   VALUE 87.                           
169200*                                                                         
190000*** END COPY WWIDFTG   LENGTH=12                                          
