000100 01  W222R89T.                                                            
000200     03 IDPTYP               PIC X(3).                                    
000300     03 KDCLAGER             PIC X.                                       
000400     03 IDARTNR              PIC X(8).                                    
000500     03 KVPB-SEP             PIC X(7).                                    
000600     03 FLMPB                PIC X.                                       
000700     03 FLOREGPB             PIC X.                                       
000800     03 TREND.                                                            
000900        05 KVTREND           PIC X(7).                                    
001000        05 FLNEGTR           PIC X.                                       
001100        05 TITREND           PIC X(4).                                    
001200        05 RVTREND           PIC X.                                       
001300     03 PB-JUSTERINGAR       OCCURS 2 TIMES.                              
001400        05 KVPB-JUST         PIC X(7).                                    
001500        05 TIPBJUST          PIC X(4).                                    
001600     03 RESEASON             OCCURS 12 TIMES                              
001700                             PIC X(2).                                    
001800     03 KDBEH-PROG           PIC 9.                                       
001900*** END OF VILMAII-COPY LENGTH= 81 BYTES                                  
