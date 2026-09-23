000010*** EDIT ALLOWED                                                          
000100 01  W221W005.                                                            
000200*        *** ORSAKSTEXT(SVENSKA) TILL NY LEVERANSPLAN (INTERN RS)         
000300*                                                                         
000400     03  MAXINDEX-1          PIC S9(3)  COMP-3  VALUE +29.                
000500     03  VARDEN-TABW005.                                                  
000600*****************************************************************         
000700       05  ORSAK-01  PIC X(8)  VALUE 'PURCHASE'.                          
000800       05  ORSAK-02  PIC X(8)  VALUE 'REG.KÖP '.                          
000900       05  ORSAK-03  PIC X(8)  VALUE 'BEKR.KÖP'.                          
001000       05  ORSAK-04  PIC X(8)  VALUE 'REG.ANN '.                          
001100       05  ORSAK-05  PIC X(8)  VALUE 'BEKR.ANN'.                          
001200       05  ORSAK-06  PIC X(8)  VALUE 'DEL.BLOC'.                          
001300       05  ORSAK-07  PIC X(8)  VALUE '        '.                          
001400       05  ORSAK-08  PIC X(8)  VALUE 'DECREASE'.                          
001500       05  ORSAK-09  PIC X(8)  VALUE 'NEW PUB '.                          
001600       05  ORSAK-10  PIC X(8)  VALUE '        '.                          
001700       05  ORSAK-11  PIC X(8)  VALUE 'NEW SUPP'.                          
001800       05  ORSAK-12  PIC X(8)  VALUE 'OMSPEC  '.                          
001900       05  ORSAK-13  PIC X(8)  VALUE 'ÄNDRAT-Q'.                          
002000       05  ORSAK-14  PIC X(8)  VALUE 'NEW SEAS'.                          
002100       05  ORSAK-15  PIC X(8)  VALUE 'NEW SCC '.                          
002200       05  ORSAK-16  PIC X(8)  VALUE 'PUBWEEKC'.                          
002300       05  ORSAK-17  PIC X(8)  VALUE 'REQ     '.                          
002400       05  ORSAK-18  PIC X(8)  VALUE 'REQ-OPT '.                          
002500       05  ORSAK-19  PIC X(8)  VALUE 'NEWDAY C'.                          
002600       05  ORSAK-20  PIC X(8)  VALUE 'X-OPT   '.                          
002700       05  ORSAK-21  PIC X(8)  VALUE 'REG.SS  '.                          
002800       05  ORSAK-22  PIC X(8)  VALUE 'RECALC  '.                          
002900       05  ORSAK-23  PIC X(8)  VALUE 'JIT     '.                          
003000       05  ORSAK-24  PIC X(8)  VALUE 'MASK.OPT'.                          
003100       05  ORSAK-25  PIC X(8)  VALUE 'NY PLAN '.                          
003200       05  ORSAK-26  PIC X(8)  VALUE 'CURRENT '.                          
003300       05  ORSAK-27  PIC X(8)  VALUE 'PLAN    '.                          
003310       05  ORSAK-28  PIC X(8)  VALUE 'WLC     '.                          
003320       05  ORSAK-29  PIC X(8)  VALUE 'CALL>2YR'.                          
003400******************************************************************        
003500     03  TABW005   REDEFINES VARDEN-TABW005.                              
003600       05  INDEX-1  OCCURS 29.                                            
003700*                   *** INDEX=KDLPORS(01-29)                              
003800          07  TELPORS            PIC X(8).                                
003900*                   *** VERBAL ORSAK TILL LEVERANSPLAN                    
004000*** END COPY W221W005C0  LENGTH=218   OLD LENGTH=202                      
