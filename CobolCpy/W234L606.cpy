000100 01  6-W234L606.                                                          
000200*                                 LÄNKAREA TILL PROGRAMMET W23460         
000300*                                 LÄSNING AV LAGERINFORMATION FÖR         
000400*                                 BESTÄMNING AV PRIORITET                 
000500     03 6-KVLS               PIC S9(7)           COMP-3                   
000600                             VALUE ZEROS.                                 
000700*                                 LAGERSALDO                              
000800     03 6-KVAKS              PIC S9(7)           COMP-3                   
000900                             VALUE ZEROS.                                 
001000*                                 ANKOMSTSALDO                            
001100     03 6-KVROS              PIC S9(7)           COMP-3                   
001200                             VALUE ZEROS.                                 
001300*                                 RESTORDERSALDO                          
001400     03 6-KVSLAGER           PIC S9(7)           COMP-3                   
001500                             VALUE ZEROS.                                 
001600*                                 SÄKERHETSLAGER                          
001700*** END COPY W234L606C0  LENGTH=16                                        
