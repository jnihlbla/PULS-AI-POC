000010 01  MED-WMEDAREA.                                                        
000020*                                 PARAMETRAR TILL WMEDKONV                
000030*                                 FÖR ATT FÅ MEDDELANDEKODER              
000040*                                 ÖVERSATTA TILL KLARTEXT.                
000050*                                 EXEMPEL PÅ ANROP:                       
000060*                                 MOVE "S  " TO MED-IDSKYLT               
000070*                                 MOVE "001" TO MED-IDMFSFEL              
000080*                                 CALL WMEDKONV USING                     
000090*                                      MED-WMEDAREA                       
000100*                                                                         
000110*                                 TEXTEN FYLLS I FÖR DET ID               
000120*                                 SOM MATATS IN.                          
000130*                                 ÖVRIGA FÄLT BLANKAS.                    
000140*                                 -------------------------------         
000150*                                 PARAMETERS TO WMEDKONV FOR              
000160*                                 TRANSLATING CODES TO TEXT.              
000170*                                 EXAMPLE OF CALL:                        
000180*                                 MOVE "GB " TO MED-IDSKYLT               
000190*                                 MOVE "001" TO MED-IDMFSINF              
000200*                                 CALL WMEDKONV USING                     
000210*                                      MED-WMEDAREA                       
000220*                                                                         
000230*                                 -------------------------------         
000240     03 MED-IDSKYLT          PIC X(3).                                    
000250*                                 NATIONALITETSTECKEN                     
000260*                                 NATIONALITY SIGN                        
000270     03 MED-MFSMED.                                                       
000280        05 MED-IDMFSMED      PIC X(3).                                    
000290*                                 MFS MEDDELANDE NUMMER                   
000300*                                 MFS MESSAGE NUMBER                      
000310        05 FILLER            PIC X.                                       
000320        05 MED-TEMFSMED      PIC X(20).                                   
000330*                                 INFO-MEDDELANDE FÖR FÄLT/RAD            
000340*                                 INFO MESSAGE FOR A FEILD/LINE           
000350     03 MED-MFSFEL.                                                       
000360        05 MED-IDMFSFEL      PIC X(3).                                    
000370*                                 MFS FELMEDDELANDE NUMMER                
000380*                                 MFS ERROR MESSAGE NUMBER                
000390        05 FILLER            PIC X.                                       
000400        05 MED-TEMFSFEL      PIC X(40).                                   
000410*                                 MFS FELMEDDELANDE                       
000420*                                 MFS ERROR MESSAGE                       
000430     03 MED-MFSINF.                                                       
000440        05 MED-IDMFSINF      PIC X(3).                                    
000450*                                 MFS INFO. MEDDELANDE NUMMER             
000460*                                 MFS INFO MESSAGE NUMBER                 
000470        05 FILLER            PIC X.                                       
000480        05 MED-TEMFSINF      PIC X(55).                                   
000490*                                 INFORMATIONSMEDDELANDE                  
000500*                                 INFORMATION MESSAGE                     
000510*** END COPY WMEDAREA  LENGTH=130                                         
