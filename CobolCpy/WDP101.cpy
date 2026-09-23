000100 01  ROT-WDP101.                                                          
000200*                                 ROTSEGMENT I SUBMIT-DATABAS             
000300*                                 FYSISK NYCKEL WDP101KY                  
000400*                                              (IDHTYP + IDRUTIN          
000500*                                               IDJOB + LOWVALUE)         
000600*                                 SÖKBEGREPP    IDHTYP, IDRUTIN,          
000700*                                               IDJOB                     
000800     03 ROT-IDHTYP           PIC X(4).                                    
000900*                                 HÄNDELSETYP                             
001000     03 ROT-IDRUTIN          PIC X(8).                                    
001100*                                 RUTINNAMN (GRUPP AV JOBB)               
001200     03 ROT-IDJOB            PIC X(8).                                    
001300*                                 JOBBNAMN                                
001400     03 ROT-LOWVALUE         PIC X(10).                                   
001500*** END COPY WDP101CCC0  LENGTH=30                                        
