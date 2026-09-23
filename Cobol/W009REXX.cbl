000010*COMPOPT STDSUB=YES                                                       
000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W009REXX.                                                 
000700 AUTHOR.        ODD OLSEN.                                                
000800     DATE-WRITTEN.  MARS 1990.                                            
000900*    REMARKS.                                                             
001000*                                                                         
001100*    FUNKTION:                                                            
001200*                                                                         
001300*    GÖR CALL PÅ ETT REXX-PROGRAM FRÅN ETT COBOLPROGRAM.                  
001400*                                                                         
001500*    SOM PARAMETER TILL REXXPROGRAMET FÅR MAN EN ADDRESS PÅ               
001600*    EN AREA I COBOLPROGRAMET.                                            
001700*                                                                         
001800*    SUBPROGRAM: IRXJCL                                                   
001900     SKIP2                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500     SKIP2                                                                
002501                                                                          
002510*    -- CHECKED BY WY2000                                                 
002600*    --- ÖVRIGR                                                           
002700 01  IRXJCL                      PIC X(8) VALUE 'IRXJCL'.                 
002800 01  PTR                         USAGE POINTER.                           
002900 01  PTR2 REDEFINES PTR          PIC S9(9) COMP.                          
003000*                                                                         
003100 01  REXX-AREA.                                                           
003200     03 KOM-LENGTH               PIC 9(4) COMP.                           
003300     03 EXEK                     PIC X(9).                                
003400     03 PARM                     PIC 9(9).                                
003500 LINKAGE SECTION.                                                         
003600 77  COMMAND                     PIC X(8).                                
003700 77  DATA-AREA                   PIC X(2000).                             
003800 PROCEDURE DIVISION USING COMMAND DATA-AREA.                              
003900     SKIP2                                                                
004000 STYR SECTION.                                                            
004100     MOVE 18 TO KOM-LENGTH                                                
004200     MOVE COMMAND TO EXEK                                                 
004300     SET PTR TO ADDRESS OF DATA-AREA                                      
004400     MOVE PTR2 TO PARM                                                    
004500     CALL IRXJCL USING REXX-AREA                                          
004600     GOBACK                                                               
004700     .                                                                    
