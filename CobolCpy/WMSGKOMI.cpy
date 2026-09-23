000010 01  MSG-KOM-WMSGKOMI.                                                    
000020*                                 BESKRIVNING AV GENERELL                 
000030*                                 IO-KOMMUNIKATIONSAREA                   
000040     03 MSG-KOM-KVLL         PIC S9(4)           COMP.                    
000050*                                 LRECL I ETT VARIABELT RECORD            
000060*                                 LRECL I A VARIABLE RECORD               
000070     03 MSG-KOM-KDZ1         PIC X.                                       
000080*                                 POS 3 I LRECL I MID/MOD                 
000090*                                 POS 3 I LRECL IN MID/MOD                
000100     03 MSG-KOM-KDZ2         PIC X.                                       
000110*                                 POS 4 I LRECL I MID/MOD                 
000120*                                 POS 4 I LRECL IN MID/MOD                
000130     03 MSG-KOM-KDTRANS      PIC X(8).                                    
000140*                                 TRANSAKTIONSKOD                         
000150*                                 TRANSACTION CODE                        
000160     03 MSG-KOM-IDCPYTXT.                                                 
000170*                                 COPYTEXT IDENTITET                      
000180*                                 IDENTITY OF A COPYTEXT                  
000190        05 MSG-KOM-CT-IDSYSTEM                                            
000200                             PIC X(4).                                    
000210*                                 SKAPANDE SYSTEMNUMMER                   
000220*                                 GENERATING SYSTEM NUMBER                
000230        05 MSG-KOM-CT-IDPTYP PIC X(3).                                    
000240*                                 POSTTYP                                 
000250*                                 RECORD TYPE                             
000260        05 MSG-KOM-CT-IDVTYP PIC X.                                       
000270*                                 POSTTYPSVERSION                         
000280*                                 RECORD TYPE VERSION                     
000290     03 MSG-KOM-IDSNDNOD     PIC X(8).                                    
000300*                                 SÄNDANDE NODE IDENTITET                 
000310*                                 IDENTITY OF SENDING NODE                
000320     03 MSG-KOM-IDSNDJOB     PIC X(8).                                    
000330*                                 SÄNDANDE JOB IDENTITET                  
000340*                                 IDENTITY OF SENDING JOB                 
000350     03 MSG-KOM-TIREGDAT     PIC 9(6).                                    
000360*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000370*                                 REGISTRATION DATE (YYMMDD)              
000380     03 MSG-KOM-TIKLOCK      PIC 9(8).                                    
000390*                                 KLOCKSLAG (TTMMSSTH)                    
000400*                                 TIME OF DAY (HHMMSSTH)                  
000410     03 MSG-KOM-IDMFSMED     PIC X(3).                                    
000420*                                 MFS MEDDELANDE NUMMER                   
000430*                                 MFS MESSAGE NUMBER                      
000440*** END COPY WMSGKOMI  LENGTH=53                                          
