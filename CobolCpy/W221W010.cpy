000010*** EDIT ALLOWED                                                          
000100 01  W221W010.                                                            
000200*             ***  KONSTANTER PER VOLYMVÄRDEKLASS          ****           
000300     03  MAXINDEX-1              PIC S9(3)  COMP-3  VALUE +5.             
000400*             ***  MAXVÄRDE FÖR INDEX                      ****           
000500     03  VARDEN-TABW010.                                                  
000600* AVSER:        VVKL-NGR   *KVAP *KVBK *KVMP *OEKORR KVQ *LPKOLL          
000700****************************************************************          
000800      05  VVKL-1 PIC X(47)                                                
000900         VALUE '00000000001*00840*00000*01600*00000*00800*00300'.         
001000      05  VVKL-2 PIC X(47)                                                
001100         VALUE '00000050000*00420*00000*00600*00000*00600*00300'.         
001200      05  VVKL-3 PIC X(47)                                                
001300         VALUE '00000500000*00420*05200*00300*00050*00200*00600'.         
001400      05  VVKL-4 PIC X(47)                                                
001500         VALUE '00005000000*00210*02600*00200*00050*00050*00300'.         
001600      05  VVKL-5 PIC X(47)                                                
001700         VALUE '00050000000*00210*02600*00150*00050*00017*00200'.         
001800******************************************************************        
001900     03  TABW010   REDEFINES VARDEN-TABW010.                              
002000      04  INDEX-1  OCCURS 5.                                              
002100*        ***   INDEX=VOLYMVÄRDEKLASS  (KDVVKL 1-5)  *************         
002200       05  PRVARDE-VVKL-NGR      DISPLAY PIC 9(9)V99.                     
002300*                            ***                                          
002400                      05  FILLER PIC X.                                   
002500       05  REKONST-KVAP          DISPLAY PIC 9(3)V99.                     
002600*                            ***                                          
002700                      05  FILLER PIC X.                                   
002800       05  REKONST-KVBK          DISPLAY PIC 9(3)V99.                     
002900*                            ***                                          
003000                      05  FILLER PIC X.                                   
003100       05  REKONST-KVMP          DISPLAY PIC 9(3)V99.                     
003200*                            ***                                          
003300                      05  FILLER PIC X.                                   
003400       05  REKONST-KVOEKORR      DISPLAY PIC 9(3)V99.                     
003500*                            ***                                          
003600                      05  FILLER PIC X.                                   
003700       05  REKONST-KVQ           DISPLAY PIC 9(3)V99.                     
003800*                            ***                                          
003900                      05  FILLER PIC X.                                   
004000       05  REKONST-LPKOLL        DISPLAY PIC 9(3)V99.                     
004100*                            ***                                          
004200*** END COPY W221W010C0  LENGTH=237   OLD LENGTH=237                      
