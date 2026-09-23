000010*** EDIT ALLOWED                                                          
000100 01  W418LTID.                                                            
000200*                      *** TABELLEN ÄR UPPBYGGD AV:                       
000300*                                                                         
000400*                      *** IDDISTR-FR.O.M.                                
000500*                      *** IDDISTR-T.O.M.                                 
000600*                      *** LIGGTID FÖR FAKTURAHISTORIK (VECKA)            
000800*                                                                         
000900*                                                                         
000910     03  VARDEN.                                                          
001000*                                                                         
001100        05  FILLER  PIC X(12)  VALUE '0001 0099 00'.                      
001200        05  FILLER  PIC X(12)  VALUE '0100 2400 06'.                      
001300        05  FILLER  PIC X(12)  VALUE '2401 9999 01'.                      
001400        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
001500        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
001600        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
001700        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
001800        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
001900        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
002000        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
002100        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
002200        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
002300        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
002400        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
002500        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
002600        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
002700        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
002800        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
002900        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
003000        05  FILLER  PIC X(12)  VALUE '0100 9999 00'.                      
004000*                                                                         
006100     03  FILLER REDEFINES VARDEN.                                         
006110        05 LTID-TABELLRAD OCCURS 20  INDEXED BY LTID-INDX.                
006200           07  LTID-IDDISTR-FOM PIC X(4).                                 
006201           07  FILLER           PIC X.                                    
006210           07  LTID-IDDISTR-TOM PIC X(4).                                 
006220           07  FILLER           PIC X.                                    
006300           07  LTID-LIGGTID-VV  PIC X(2).                                 
