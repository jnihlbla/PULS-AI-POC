000010*** EDIT ALLOWED                                                          
000001*      LEDTEXTER ONLY IN ENGLISH                                          
000090*                                                                         
000100 01  W476W001.                                                            
000300*                                                                         
000300*      1=SVENSKA, 2=ENGELSKA,3=FRANSKA,4=SPANSKA                          
000300*      5=TYSKA,   6=ITALIENS,7=FLAMLÄN,8=LEDIGT                           
000300*                                                                         
000200*                                ***  DEALER LEDTEXTER                    
000300*                                                                         
000900    04  IDKUNDNR-LEDTEXTER.                                               
001000       05  FILLER                PIC X(06) VALUE 'DEALER'.                
001000       05  FILLER                PIC X(06) VALUE 'DEALER'.                
001000       05  FILLER                PIC X(06) VALUE 'DEALER'.                
001000       05  FILLER                PIC X(06) VALUE 'DEALER'.                
001000       05  FILLER                PIC X(06) VALUE 'DEALER'.                
001000       05  FILLER                PIC X(06) VALUE 'DEALER'.                
001000       05  FILLER                PIC X(06) VALUE 'DEALER'.                
001000       05  FILLER                PIC X(06) VALUE 'DEALER'.                
001600    04  FILLER  REDEFINES  IDKUNDNR-LEDTEXTER.                            
001700       05  IDKUNDNR-LEDTEXT      PIC X(06)  OCCURS 8.                     
001710*                                                                         
000300*                                                                         
000200*                                ***  CURRENCY LEDTEXTER                  
000300*                                                                         
000900    04  KDVALISO-LEDTEXTER.                                               
001000       05  FILLER              PIC X(08) VALUE 'CURRENCY'.                
001000       05  FILLER              PIC X(08) VALUE 'CURRENCY'.                
001000       05  FILLER              PIC X(08) VALUE 'CURRENCY'.                
001000       05  FILLER              PIC X(08) VALUE 'VALOR   '.                
001000       05  FILLER              PIC X(08) VALUE 'CURRENCY'.                
001000       05  FILLER              PIC X(08) VALUE 'CURRENCY'.                
001000       05  FILLER              PIC X(08) VALUE 'CURRENCY'.                
001000       05  FILLER              PIC X(08) VALUE 'CURRENCY'.                
001600    04  FILLER  REDEFINES  KDVALISO-LEDTEXTER.                            
001700       05  KDVALISO-LEDTEXT    PIC X(08)  OCCURS 8.                       
001710*                                                                         
000300*                                                                         
000200*                                *** IDKUNDRF-RO LEDTEXTER                
000300*                                                                         
000900    04  IDKUNDRF-RO-LEDTEXTER.                                            
001000       05  FILLER              PIC X(10) VALUE 'BO.ORD.NR '.              
001000       05  FILLER              PIC X(10) VALUE 'BO.ORD.NO '.              
001000       05  FILLER              PIC X(10) VALUE 'BO.ORD.NO '.              
001000       05  FILLER              PIC X(10) VALUE 'BO.ORD.NO '.              
001000       05  FILLER              PIC X(10) VALUE 'BO.ORD.NO '.              
001000       05  FILLER              PIC X(10) VALUE 'BO.ORD.NO '.              
001000       05  FILLER              PIC X(10) VALUE 'BO.ORD.NO '.              
001000       05  FILLER              PIC X(10) VALUE 'BO.ORD.NO '.              
001600    04  FILLER  REDEFINES  IDKUNDRF-RO-LEDTEXTER.                         
001700       05  IDKUNDRF-RO-LEDTEXT PIC X(10)  OCCURS 8.                       
001710*                                                                         
000300*                                                                         
000200*                                ***  TOTAL PER DISTRICT                  
000300*                                     LEDTEXTER                           
000300*                                                                         
000900    04  IDSHIPM-TOTAL-LEDTEXTER.                                          
001000       05  FILLER              PIC X(18)                                  
001000                               VALUE 'TOTAL PER DISTRICT'.                
001000       05  FILLER              PIC X(18)                                  
001000                               VALUE 'TOTAL PER DISTRICT'.                
001000       05  FILLER              PIC X(18)                                  
001000                               VALUE 'TOTAL PER DISTRICT'.                
001000       05  FILLER              PIC X(18)                                  
001000                               VALUE 'DISTRITO POR TOTAL'.                
001000       05  FILLER              PIC X(18)                                  
001000                               VALUE 'TOTAL PER DISTRICT'.                
001000       05  FILLER              PIC X(18)                                  
001000                               VALUE 'TOTAL PER DISTRICT'.                
001000       05  FILLER              PIC X(18)                                  
001000                               VALUE 'TOTAL PER DISTRICT'.                
001000       05  FILLER              PIC X(18)                                  
001000                               VALUE 'TOTAL PER DISTRICT'.                
001600    04  FILLER  REDEFINES  IDSHIPM-TOTAL-LEDTEXTER.                       
001700       05  IDSHIPM-TOTAL-LEDTEXT PIC X(18)   OCCURS 8.                    
001710*                                                                         
000200*                                *** RADNR LEDTEXTER                      
000300*                                                                         
000900    04  RADNR-LEDTEXTER.                                                  
001000       05  FILLER              PIC X(05) VALUE 'LINE '.                   
001000       05  FILLER              PIC X(05) VALUE 'LINE '.                   
001000       05  FILLER              PIC X(05) VALUE 'LINE '.                   
001000       05  FILLER              PIC X(05) VALUE 'LINE '.                   
001000       05  FILLER              PIC X(05) VALUE 'LINIE'.                   
001000       05  FILLER              PIC X(05) VALUE 'LINE '.                   
001000       05  FILLER              PIC X(05) VALUE 'LINE '.                   
001000       05  FILLER              PIC X(05) VALUE 'LINE '.                   
001600    04  FILLER  REDEFINES  RADNR-LEDTEXTER.                               
001700       05  RADNR-LEDTEXT       PIC X(05)  OCCURS 8.                       
001710*                                                                         
000200*                                *** TELEFON LEDTEXTER                    
000300*                                                                         
000900    04  TELEF-LEDTEXTER.                                                  
001000       05  FILLER            PIC X(10) VALUE 'TELEPHONE:'.                
001000       05  FILLER            PIC X(10) VALUE 'TELEPHONE:'.                
001000       05  FILLER            PIC X(10) VALUE 'TELEPHONE:'.                
001000       05  FILLER            PIC X(10) VALUE 'TELF. :   '.                
001000       05  FILLER            PIC X(10) VALUE 'TELEFON : '.                
001000       05  FILLER            PIC X(10) VALUE 'TELEPHONE:'.                
001000       05  FILLER            PIC X(10) VALUE 'TELEPHONE:'.                
001000       05  FILLER            PIC X(10) VALUE 'TELEPHONE:'.                
001600    04  FILLER  REDEFINES  TELEF-LEDTEXTER.                               
001700       05  TELEF-LEDTEXT       PIC X(10)  OCCURS 8.                       
001710*                                                                         
000200*                                *** FAX LEDTEXTER                        
000300*                                                                         
000900    04  FAX-LEDTEXTER.                                                    
001000       05  FILLER              PIC X(05) VALUE 'FAX: '.                   
001000       05  FILLER              PIC X(05) VALUE 'FAX: '.                   
001000       05  FILLER              PIC X(05) VALUE 'FAX: '.                   
001000       05  FILLER              PIC X(05) VALUE 'FAX: '.                   
001000       05  FILLER              PIC X(05) VALUE 'FAX: '.                   
001000       05  FILLER              PIC X(05) VALUE 'FAX: '.                   
001000       05  FILLER              PIC X(05) VALUE 'FAX: '.                   
001000       05  FILLER              PIC X(05) VALUE 'FAX: '.                   
001600    04  FILLER  REDEFINES  FAX-LEDTEXTER.                                 
001700       05  FAX-LEDTEXT         PIC X(05)  OCCURS 8.                       
001710*                                                                         
001800*** END COPY W476W001    LENGTH=      OLD LENGTH=                         
