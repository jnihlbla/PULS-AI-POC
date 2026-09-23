000010 01  KOM-WDP801.                                                          
000020*                                 KOMMUNIKATIONSREGISTER                  
000030*                                 KOMMUNIKATIONSSEGMENT                   
000040*                                 FYSISK NYCKEL: WDP801KY                 
000050*                                 (IDSNDNOD + IDSNDJOB                    
000060*                                 (+ TIREGDAT + TIKLOCK                   
000070     03 KOM-IDSNDNOD         PIC X(8).                                    
000080*                                 SÄNDANDE NODE IDENTITET                 
000090*                                 IDENTITY OF SENDING NODE                
000100     03 KOM-IDSNDJOB         PIC X(8).                                    
000110*                                 SÄNDANDE JOB IDENTITET                  
000120*                                 IDENTITY OF SENDING JOB                 
000130     03 KOM-TIREGDAT         PIC S9(7)           COMP-3.                  
000140*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000150*                                 REGISTRATION DATE (YYMMDD)              
000160     03 KOM-TIKLOCK          PIC S9(9)           COMP-3.                  
000170*                                 KLOCKSLAG (TTMMSSTH)                    
000180*                                 TIME OF DAY (HHMMSSTH)                  
000190     03 KOM-IDCPYTXT.                                                     
000200*                                 COPYTEXT IDENTITET                      
000210*                                 IDENTITY OF A COPYTEXT                  
000220        05 KOM-CT-IDSYSTEM   PIC X(4).                                    
000230*                                 SKAPANDE SYSTEMNUMMER                   
000240*                                 GENERATING SYSTEM NUMBER                
000250        05 KOM-CT-IDPTYP     PIC X(3).                                    
000260*                                 POSTTYP                                 
000270*                                 RECORD TYPE                             
000280        05 KOM-CT-IDVTYP     PIC X.                                       
000290*                                 POSTTYPSVERSION                         
000300*                                 RECORD TYPE VERSION                     
000310     03 KOM-IDLTERM          PIC X(8).                                    
000320*                                 LOGISKT TERMINALNAMN                    
000330*                                 IDENTITY OF LOGICAL TERMINAL            
000340     03 KOM-IDMFSMED         PIC X(3).                                    
000350*                                 MFS MEDDELANDE NUMMER                   
000360*                                 MFS MESSAGE NUMBER                      
000370     03 KOM-IDUSER           PIC X(8).                                    
000380*                                 ANVÄNDARENS SÄKERHETS ID                
000390*                                 USER SECURITY-IDENTITY                  
000400     03 KOM-KDKOMSTA         PIC X.                                       
000410*                                 KOMMUNIKATIONSSTATUS                    
000420*                                 COMMUNICATION STATUS                    
000430     03 KOM-KDKOMBEH         PIC X.                                       
000440*                                 DISPATCHER FELHANTERING                 
000450*                                 DISPATCHER ERROR HANDLING               
000460*** END COPY WDP801    LENGTH=54                                          
