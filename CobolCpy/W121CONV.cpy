000100*** EDIT ALLOWED                                                          
000200*   Tecken-Shift-Konvertering i Svensk EBCDIC                             
000300*                                                                         
000400*        Ex1. All text till VERSALER                                      
000500*             INSPECT WS-TEXT CONVERTING GEMEN TO VERSAL                  
000600*        Ex2. Den fˆrsta bokstaven skall vara VERSAL                      
000700*             INSPECT WS-TEXT(1:1) CONVERTING GEMEN TO VERSAL             
000800*        Ex3. Alla utom den fˆrsta skall vara gemener.                    
000900*             INSPECT WS-TEXT(2:) CONVERTING VERSAL TO GEMEN              
001000*                                                                         
001100 01  GEMEN                       PIC X(54)                                
001200            VALUE 'abcdefghijklmnopqrstuvwxyzÂ‰ˆ¸¯‚‡·„ÁÍÈÎËÌÓÔÏÒÊ˝        
001300-          '˚˘˙ÛÚı'.                                                     
001400 01  VERSAL                      PIC X(54)                                
001500            VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ≈ƒ÷‹ÿ¬¿¡√« …À»ÕŒœÃ—∆›        
001600-          '€Ÿ⁄”“’–'.                                                     
