010000*** EDIT ALLOWED                                                          
020000*                            *************************************        
030000*                            *** ANVÄNDS FÖR ATT SKILJA BYTESRADIO        
040000*                            *** FRÅN ÖVRIGA BYTESENHETER                 
050002*                            *** INNEHÅLLER RENOVERADE ENHETER            
060006*                            *** OBS ! DETTA ÄR ARTIKELNUMMER             
070006*                            *** FÖR RENOVERAT OBJEKT                     
080000*                            *************************************        
090002 01  BYT19-IDARTNR           PIC 9(9)    COMP-3.                          
100000*                                                                         
110002       88  BYT19-BYTES       VALUE  5001000 THRU 5003999                  
120002                                    8111000 THRU 8113999                  
130005                                    8251000 THRU 8253999                  
140002                                    8601000 THRU 8603999                  
150007                                    9031000 THRU 9031999                  
160010                                   31240000 THRU 31243999                 
160020                                   36000000 THRU 36003999                 
160020                                   36010000 THRU 36013999                 
160020                                   36020000 THRU 36023999                 
160020                                   36050000 THRU 36053999                 
                                         36081000 THRU 36083999.                
100000*                                                                         
170002       88  BYT19-RADIO       VALUE  8103000 THRU 8103999                  
180002                                    9953000 THRU 9953999                  
180003                                   36100000 THRU 36100999.                
190003*** END COPY WWBYT19                                                      
