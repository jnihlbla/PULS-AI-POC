000100 01  4495-WDGX4495.                                                       
000200*                                 LASTBÄRARE                              
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (IDHTYP   + IDDC    +                   
000500*                                 (IDTRPTNR + IDLBBET +                   
000600*                                  LOW-VALUE)                             
000700     03 4495-IDHTYP          PIC X(4).                                    
000800*                                 HÄNDELSETYP                             
000900     03 4495-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 4495-IDTRPTNR        PIC S9(3)           COMP-3.                  
001300*                                 TRANSPORTIDENTITET                      
001400*                                 TRANSPORT IDENTITY                      
001500     03 4495-IDLBBET         PIC X(12).                                   
001600*                                 LASTBÄRARBETECKNING                     
001700*                                 TRAILER NUMBER                          
001800     03 4495-LOW-VALUE       PIC X(10).                                   
001900*** END COPY WDGX4495    LENGTH=30                                        
