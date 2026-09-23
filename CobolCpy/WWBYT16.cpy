001000*** EDIT ALLOWED                                                          
010000*                          ***************************************        
020000*                          *** ANVÄNDS FÖR ATT SKILJA                     
020100*                          *** ALLA RADIO-BYTES-ENHETER                   
030000*                          *** FRÅN ALLA ANDRA BYTES-ENHETER              
032002*                          *** BYTES-ENHETER = RENOVERADE + OBJEKT        
040000*                          ***************************************        
050000 01  BYT16-IDARTNR           PIC 9(9)    COMP-3.                          
060000*                                                                         
070000       88  BYT16-BYTES       VALUE  5001000 THRU 5009999                  
080000                                    8111000 THRU 8119999                  
081006                                    8251000 THRU 8259999                  
090000                                    8601000 THRU 8609999                  
100005                                    9031000 THRU 9031999                  
101007                                    9037000 THRU 9037999                  
102007                                   31240000 THRU 31249999                 
102008                                   36000000 THRU 36009999                 
102009                                   36010000 THRU 36019999                 
102009                                   36020000 THRU 36029999                 
102009                                   36050000 THRU 36059999                 
                                         36081000 THRU 36089999.                
060000*                                                                         
110001       88  BYT16-RADIO       VALUE  8103000 THRU 8104999                  
111001                                    9953000 THRU 9954999                  
111002                                   36100000 THRU 36101999.                
060000*                                                                         
111003       88  BYT16-RADIO-EXTRA VALUE 36100000 THRU 36101999.                
120000*** END COPY WWBYT01CC0  LENGTH=5     OLD LENGTH=0                        
