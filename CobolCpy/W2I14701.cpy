000100 01  MID-W2I14701.                                                        
000200     03 MID-IDANSK-IN        PIC X(3).                                    
000300*                                 PROCURER NO.                            
000400     03 MID-IDANSK-TO-IN     PIC X(3).                                    
000500*                                 PROCURER NO.                            
000600     03 MID-IDLEVNR-IN       PIC X(5).                                    
000700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
000800     03 MID-KDLPORS-IN       PIC X(2).                                    
000900     03 MID-KDLEVPLF-IN      PIC X.                                       
001000*                                 CODE FOR APPROVAL OF SCHEDULE P         
001100*                                 ROPOSAL                                 
001200     03 MID-RAD              OCCURS 15 TIMES.                             
001300        05 MID-KDCMDVAL      PIC X.                                       
001400*                                 GENERAL COMMAND-CODE                    
001500        05 MID-IDARTNR       PIC X(8).                                    
001600*                                 PART NUMBER                             
001700        05 MID-IDLEVNR       PIC X(5).                                    
001800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001900*** END OF VILMAII-COPY LENGTH= 224 BYTES                                 
