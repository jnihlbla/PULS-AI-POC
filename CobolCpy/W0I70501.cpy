000100 01  MID-W0I70501.                                                        
000200*                                 COPYTEXT FÖR MID W0I70501               
000300     03 MID-IDRUTIN-IN       PIC X(8).                                    
000400*                                 RUTINNAMN (GRUPP AV JOBB)               
000500     03 MID-IDRUTIN-UT       PIC X(8).                                    
000600*                                 RUTINNAMN (GRUPP AV JOBB)               
000700     03 MID-IDJOB-IN         PIC X(8).                                    
000800*                                 JOBBNAMN                                
000900     03 MID-IDJOB-UT         PIC X(8).                                    
001000*                                 JOBBNAMN                                
001100     03 MID-FLKLAR           PIC X.                                       
001200*                                 AVSLUTNINGSMARKERING                    
001300     03 MID-IDJCLRAD-IN      PIC 9(5).                                    
001400*                                 RADNUMMER PÅ JCL                        
001500     03 MID-LINES            OCCURS 14 TIMES                              
001600                             INDEXED MID-IX-LINE.                         
001700        05 MID-IDJCLRAD      PIC 9(5).                                    
001800*                                 RADNUMMER PÅ JCL                        
001900        05 MID-TEJCL         PIC X(71).                                   
002000*                                 JCL-KORT                                
002100*** END COPY W0I70501C0  LENGTH=1102                                      
