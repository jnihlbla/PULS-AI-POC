000100 01  WCNVAREA.                                                            
000200*                                 LINK AREA TO ONE OF THE SUB             
000300*                                 PROGRAMS WCNVXXXX.                      
000400*                                 CONVERSION OF TEXT BETWEEN              
000500*                                 DIFFERENT CCS-ID:S                      
000600*                                                                         
000700*                                 INPUT ARGUMENTS:                        
000800*                                  TECONV-FROM   INPUT TEXT.              
000900*                                  FLTXTENT      J IF SGML TEXT-          
001000*                                    ENTITIES SHOULD BE PRODUCED,         
001100*                                    ELSE N.  ONLY RELEVANT WHEN          
001200*                                    CONVERTING TO UNICODE.               
001300*                                  KVMAXTL       MAX ALLOWED              
001400*                                    OUTPUT TEXT LENGTH.                  
001500*                                  KDBYTEORD     BYTE ORDER CODE          
001600*                                    (M=MSB/MOTOROLA L=LSB/INTEL)         
001700*                                    ONLY RELEVANT WHEN CONV.             
001800*                                    FROM UNICODE.                        
001900*                                  FLUTF8        J IF INPUT IS IN         
002000*                                                COMPACT UTF8             
002100*                                                FORMAT, N IF IN          
002200*                                                FULL UCS2 FMT.           
002300*                                                                         
002400*                                 OUTPUT ARGUMENTS:                       
002500*                                  TECONV-TO     CONVERTED TEXT           
002600*                                  KDSVAR        BLANK = OK               
002700*                                                T = OUTPUT HAS           
002800*                                                BEEN TRUNCATED.          
002900*                                                F = INVALID              
003000*                                                CHARS IN INPUT.          
003100*                                  BEFEL         ERROR TEXT               
003200     03 TECONV-FROM          PIC X(700).                                  
003300*                                 TEXT SOM SKA KONVERTERAS                
003400*                                 TEXT TO BE CONVERTED                    
003500     03 FLTXTENT             PIC X.                                       
003600*                                 PRODUCERA TEXT-ENTITIES?                
003700*                                 PRODUCE TEXT ENTITIES?                  
003800     03 KVMAXTL              PIC S9(3)           COMP-3.                  
003900*                                 MAX TILLÅTEN TEXTLÄNGD                  
004000*                                 MAX ALLOWED TEXT LENGTH                 
004100     03 KDBYTEORD            PIC X.                                       
004200*                                 BYTEORDNING, MSB / LSB                  
004300*                                 BYTE ORDER, MSB / LSB                   
004400     03 TECONV-TO            PIC X(700).                                  
004500*                                 TEXT SOM HAR KONVERTERAS                
004600*                                 TEXT WHICH HAS BEEN CONVERTED           
004700     03 KDSVAR               PIC X.                                       
004800*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
004900*                                 RETURN CODE FROM PROGRAM                
005000     03 BEFEL                PIC X(50).                                   
005100*                                 FELTEXT                                 
005200*                                 ERROR TEXT                              
005300     03 FLUTF8               PIC X.                                       
005400*                                 UTF8-FORMAT?                            
005500*                                 UTF8-FORMAT?                            
005600*** END OF VILMAII-COPY LENGTH= 1456 BYTES                                
