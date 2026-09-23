000100 01  0-W221L800.                                                          
000200*                                 LÄNKAREA TILL PROGRAMMET W22180         
000300     03 0-KDCALL             PIC S9(3)           COMP-3                   
000400                             VALUE ZEROS.                                 
000500*                                 ANROPSTYP                               
000600     03 0-LAES-WLXXBK01      PIC S9(3)           COMP-3                   
000700                             VALUE +1.                                    
000800     03 0-LAES-WLXXBK11      PIC S9(3)           COMP-3                   
000900                             VALUE +2.                                    
001000     03 0-REPL-WLXXBK11      PIC S9(3)           COMP-3                   
001100                             VALUE +3.                                    
001200     03 0-LAES-WLXXBL01      PIC S9(3)           COMP-3                   
001300                             VALUE +4.                                    
001400     03 0-LAES-WLXXBL11-KVAL PIC S9(3)           COMP-3                   
001500                             VALUE +5.                                    
001600     03 0-DLET-WLXXBL11      PIC S9(3)           COMP-3                   
001700                             VALUE +6.                                    
001800     03 0-LAES-WLARTC01      PIC S9(3)           COMP-3                   
001900                             VALUE +7.                                    
002000     03 0-IDLEVNR            PIC X(5)                                     
002100                             VALUE SPACES.                                
002200*                                 LEVERANTÖRNUMMER                        
002300     03 0-IDARTNR            PIC S9(9)           COMP-3                   
002400                             VALUE ZEROS.                                 
002500*                                 ARTIKELNUMMER                           
002600     03 0-KDSVAR             PIC X                                        
002700                             VALUE SPACE.                                 
002800*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002900     03 0-KDSVAR-OK          PIC X                                        
003000                             VALUE ' '.                                   
003100*                                 SVARSKOD : OK                           
003200     03 0-KDSVAR-FEL         PIC X                                        
003300                             VALUE 'F'.                                   
003400*                                 SVARSKOD: FEL                           
003500*                                                                         
003600*** END OF VILMAII-COPY LENGTH= 29 BYTES                                  
