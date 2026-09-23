000010 01  SEQA-WDP8A1.                                                         
000020*                                 SEKUNDÄRT INDEX TILL WDP801             
000030*                                 KOMMUNIKATIONSREGISTER                  
000040*                                 FYSISK NYCKEL: WDP8A1KY                 
000050*                                 (TIREGDAT + TIKLOCK                     
000060*                                  + IDSNDNOD + IDSNDJOB)                 
000070*                                 SEKUNDÄR NYCKEL: WDP8ASEQ               
000080*                                 (TIREGDAT + TIKLOCK)                    
000090     03 SEQA-TIREGDAT        PIC S9(7)           COMP-3.                  
000100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000110*                                 REGISTRATION DATE (YYMMDD)              
000120     03 SEQA-TIKLOCK         PIC S9(9)           COMP-3.                  
000130*                                 KLOCKSLAG (TTMMSSTH)                    
000140*                                 TIME OF DAY (HHMMSSTH)                  
000150     03 SEQA-IDSNDNOD        PIC X(8).                                    
000160*                                 SÄNDANDE NODE IDENTITET                 
000170*                                 IDENTITY OF SENDING NODE                
000180     03 SEQA-IDSNDJOB        PIC X(8).                                    
000190*                                 SÄNDANDE JOB IDENTITET                  
000200*                                 IDENTITY OF SENDING JOB                 
000210     03 SEQA-IDCPYTXT.                                                    
000220*                                 COPYTEXT IDENTITET                      
000230*                                 IDENTITY OF A COPYTEXT                  
000240        05 SEQA-CT-IDSYSTEM  PIC X(4).                                    
000250*                                 SKAPANDE SYSTEMNUMMER                   
000260*                                 GENERATING SYSTEM NUMBER                
000270        05 SEQA-CT-IDPTYP    PIC X(3).                                    
000280*                                 POSTTYP                                 
000290*                                 RECORD TYPE                             
000300        05 SEQA-CT-IDVTYP    PIC X.                                       
000310*                                 POSTTYPSVERSION                         
000320*                                 RECORD TYPE VERSION                     
000330     03 SEQA-IDLTERM         PIC X(8).                                    
000340*                                 LOGISKT TERMINALNAMN                    
000350*                                 IDENTITY OF LOGICAL TERMINAL            
000360     03 SEQA-IDMFSMED        PIC X(3).                                    
000370*                                 MFS MEDDELANDE NUMMER                   
000380*                                 MFS MESSAGE NUMBER                      
000390     03 SEQA-IDUSER          PIC X(8).                                    
000400*                                 ANVÄNDARENS SÄKERHETS ID                
000410*                                 USER SECURITY-IDENTITY                  
000420     03 SEQA-KDKOMSTA        PIC X.                                       
000430*                                 KOMMUNIKATIONSSTATUS                    
000440*                                 COMMUNICATION STATUS                    
000450     03 SEQA-KDKOMBEH        PIC X.                                       
000460*                                 DISPATCHER FELHANTERING                 
000470*                                 DISPATCHER ERROR HANDLING               
000480*** END COPY WDP8A1    LENGTH=54                                          
