000010*** EDIT ALLOWED                                                          
000100*       +------------------------------------------------+                
000200*       !   KVALITETSUPPFÖLJNING FÖR DC=23 (G.BRITAIN)   !                
000300*       !               - LAGEROMRÅDE                    !                
000400*       !               - BENÄMNING LAGEROMRÅDE, (GB)    !                
000410*       !               - BENÄMNING LAGEROMRÅDE, (GB)    !                
000500*       !               - OMRÄKNINGSTAL                  !                
000600*       !               - ANTAL URVALSGRUPPER            !                
000700*       +------------------------------------------------+                
000810                                                                          
000900       01 DC23-ENTRESOLEN.                                                
001000          03 FILLER        PIC X(01)        VALUE 'A'.                    
001100          03 FILLER        PIC X(10)        VALUE 'MEZZANINE '.           
001300          03 FILLER        PIC X(10)        VALUE 'MEZZANINE '.           
001400          03 FILLER        PIC 9(02)V9(01)  VALUE 6.8  .                  
001600          03 FILLER        PIC 9(01)        VALUE 4    .                  
001800       01 DC23-PALL.                                                      
001900          03 FILLER        PIC X(01)        VALUE 'B'.                    
002000          03 FILLER        PIC X(10)        VALUE 'PALLET    '.           
002200          03 FILLER        PIC X(10)        VALUE 'PALLET    '.           
002300          03 FILLER        PIC 9(02)V9(01)  VALUE 3.0  .                  
002500          03 FILLER        PIC 9(01)        VALUE 3    .                  
002700       01 DC23-OVRIG-PALL.                                                
002800          03 FILLER        PIC X(01)        VALUE 'C'.                    
002900          03 FILLER        PIC X(10)        VALUE 'REMAINING '.           
003100          03 FILLER        PIC X(10)        VALUE 'REMAINING '.           
003300          03 FILLER        PIC 9(02)V9(01)  VALUE 9.0  .                  
003500          03 FILLER        PIC 9(01)        VALUE 1    .                  
003510       01 DC23-GROV.                                                      
003520          03 FILLER        PIC X(01)        VALUE 'D'.                    
003530          03 FILLER        PIC X(10)        VALUE 'REMAINING '.           
003550          03 FILLER        PIC X(10)        VALUE 'REMAINING '.           
003570          03 FILLER        PIC 9(02)V9(01)  VALUE 3.7  .                  
003590          03 FILLER        PIC 9(01)        VALUE 3    .                  
003591       01 DC23-72.                                                        
003592          03 FILLER        PIC X(01)        VALUE 'E'.                    
003595          03 FILLER        PIC X(10)        VALUE '72        '.           
003596          03 FILLER        PIC X(10)        VALUE '72        '.           
003597          03 FILLER        PIC 9(02)V9(01)  VALUE 3.0  .                  
003599          03 FILLER        PIC 9(01)        VALUE 2    .                  
003601       01 DC23-SPECIAL.                                                   
003602          03 FILLER        PIC X(01)        VALUE 'F'.                    
003603          03 FILLER        PIC X(10)        VALUE 'SPECIAL   '.           
003604          03 FILLER        PIC X(10)        VALUE 'SPECIAL   '.           
003607          03 FILLER        PIC 9(02)V9(01)  VALUE 6.8  .                  
003609          03 FILLER        PIC 9(01)        VALUE 2  .                    
