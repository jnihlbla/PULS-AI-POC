000010*** EDIT ALLOWED                                                          
000010*****************************************************************         
000100*                                 TABELL                                  
000200*                                 VOLYMVÄRDESKLASS                        
000300*                                 PER PRIS- OCH FREKVENS-KLASS            
000310*****************************************************************         
000400    01  TAB-VVKL.                                                         
000500        03  FILLER PIC X(7)  VALUE '1122334'.                             
000600        03  FILLER PIC X(7)  VALUE '1223344'.                             
000700        03  FILLER PIC X(7)  VALUE '2233445'.                             
000800        03  FILLER PIC X(7)  VALUE '2334455'.                             
000900        03  FILLER PIC X(7)  VALUE '3344555'.                             
001000        03  FILLER PIC X(7)  VALUE '3445555'.                             
001100        03  FILLER PIC X(7)  VALUE '4455555'.                             
001200        03  FILLER PIC X(7)  VALUE '4555555'.                             
001300        03  FILLER PIC X(7)  VALUE '5555555'.                             
001310                                                                          
001400    01  FILLER REDEFINES TAB-VVKL.                                        
001500        03  TAB-PRISKL  OCCURS 9.                                         
001600            05  TAB-FREKKL  OCCURS 7.                                     
001700                07  TAB-KDVVKL  PIC 9.                                    
