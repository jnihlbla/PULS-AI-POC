000100 01  W6115A01.                                                            
000200*                                 LEAD TIME DATA FOR INBOUND PER          
000300*                                 PLACEMENT IN DIFFERENT IDDC             
000400*                                 AIM TIMES FOR WORKFLOW                  
000500     03 IDDC                 PIC X(2).                                    
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 KDINLUPF             PIC X(4).                                    
000800*                                 FOLLOW-UP STATUS RECEIVING              
000900     03 KVTID-NORM           PIC 9(4).                                    
001000*                                 NORMAL AIM TIME FOR AN ADDRESS          
001100     03 KVTID-PRIO           PIC 9(4).                                    
001200*                                 PRIO AIM TIME WORKFLOW/ADDRESS          
001300     03 FLEXCP               PIC X.                                       
001400*                                 GENERAL FLAG FOR EXCEPTION              
001500     03 IDLEVNR              PIC X(5).                                    
001600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001700     03 IDAVD-DAG            PIC X(5).                                    
001800*                                 DEPARTMENT OF EMPLOYED/DAY              
001900     03 IDGRUPP-DAG          PIC X(2).                                    
002000*                                 TEAM ID OF EMPLOYED/DAY                 
002100     03 IDAVD-NATT           PIC X(5).                                    
002200*                                 DEPARTMENT OF EMPLOYED/NIGHT            
002300     03 IDGRUPP-NATT         PIC X(2).                                    
002400*                                 TEAM ID OF EMPLOYED/NIGHT               
002500*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
