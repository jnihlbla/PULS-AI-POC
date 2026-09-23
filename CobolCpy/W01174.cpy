000100 01  W01174.                                                              
000200*                                 UTDRAG UR WDD301                        
000300*                                                                         
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 BEART-TAB            OCCURS 10 TIMES.                             
000700        05 IDSKYLT           PIC X(3).                                    
000800*                                 NATIONALITETSTECKEN                     
000900*                                 SPRÅKIDENTIFIKATION                     
001000        05 BEART             PIC X(25).                                   
001100*                                 ARTIKELBENÄMNING                        
001200*** END COPY W01174      LENGTH=285                                       
