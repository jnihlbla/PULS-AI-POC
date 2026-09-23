000100 01  MID-W0I70701.                                                        
000200*                                 COPYTEXT FÖR MID W0I70701               
000300     03 MID-IDRUTIN-IN       PIC X(8).                                    
000400*                                 RUTINNAMN (GRUPP AV JOBB)               
000500     03 MID-IDRUTIN-UT       PIC X(8).                                    
000600*                                 RUTINNAMN (GRUPP AV JOBB)               
000700     03 MID-IDJOB-IN         PIC X(8).                                    
000800*                                 JOBBNAMN                                
000900     03 MID-IDJOB-UT         PIC X(8).                                    
001000*                                 JOBBNAMN                                
001100     03 MID-IDJCLRAD-IN      PIC 9(5).                                    
001200*                                 RADNUMMER PÅ JCL                        
001300     03 MID-LINES            OCCURS 14 TIMES                              
001400                             INDEXED MID-IX-LINE.                         
001500        05 MID-IDJCLRAD      PIC 9(5).                                    
001600*                                 RADNUMMER PÅ JCL                        
001700        05 MID-TEJCL         PIC X(71).                                   
001800*                                 JCL-KORT                                
001900*** END COPY W0I70701C0  LENGTH=1101                                      
