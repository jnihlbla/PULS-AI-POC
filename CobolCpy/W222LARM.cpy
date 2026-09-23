000010*** EDIT ALLOWED                                                          
000100 01  W222LARM.                                                            
000200*                                 TABELL LARMFAKTORER CDC.                
000300*                                 ANVÄNDS FÖR ATT LARMA OM STORA          
000400*                                 ELLER SMÅ UTTAG                         
000500*                                                                         
000700        05 FILLER PIC X(10) VALUE '1210277018'.                           
000800        05 FILLER PIC X(10) VALUE '2210277018'.                           
001000        05 FILLER PIC X(10) VALUE '3139277018'.                           
001100        05 FILLER PIC X(10) VALUE '4104185037'.                           
001200        05 FILLER PIC X(10) VALUE '5092139046'.                           
003300                                                                          
013209                                                                          
013210 01  FILLER REDEFINES W222LARM.                                           
013400        05 FILLER            OCCURS 5.                                    
013500           07 LARMGRAENSER.                                               
013600               09 LARM-KDVVKL     PIC 9.                                  
013610               09 LARM-2V-HOEG    PIC 9V9(2).                             
013700               09 LARM-4V-HOEG    PIC 9V9(2).                             
013800               09 LARM-4V-LAAG    PIC 9V9(2).                             
014100                                                                          
000063 01  LARM-MAX-IX                  PIC S9(3) VALUE 5.                      
