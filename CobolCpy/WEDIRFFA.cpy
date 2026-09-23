000010*** EDIT ALLOWED                                                          
000011                                                                          
000012*    EDI RFF REFERENCE                                                    
000013*    ANVÄNDS FÖR DESDAV MEDDELANDE, GENERELL COPYTEXT                     
000014*                                                                         
000015*    FUNCTION,                                                            
000016*    -A GROUP OF SEG. TO IDENTIFY REF TO A GOODS ITEM                     
000018*                                                                         
000040*                                                                         
000100 01  WEDIRFF.                                                             
000230     03 RFF-IDPTYP                             PIC X(03).                 
000240*                                              RFF                        
000250     03 RFF-LENGTH                             PIC 9(03).                 
000260*                                              LENGTH = 073               
000503*                                                                         
000504*                                                                         
000515     03 RFF-C506-REFERENCE.                                               
000516*                                                                         
000517        05 RFF-1153-REFERENCE-QUAL             PIC X(03).                 
000518*       CU = DISTRICT                                                     
000519*       ON = ORDER NO.                                                    
000521*       AAT= CASE NO.                                                     
000522*       CR = CUSTOMER REF.                                                
000523*                                                                         
000524        05 RFF-1154-REFERENCE-NO               PIC X(70).                 
000525*       CU = DISTRICT NO  (4 BYTES)                                       
000526*       ON = ORDER NO.    (5 BYTES)                                       
000527*       AAT= CASE NO.     (5 BYTES)                                       
000528*       CR = CUSTOMER REF.(15 BYTES)                                      
000529*                                                                         
000530*** END OF VILMAII-COPY LENGTH=79                                         
