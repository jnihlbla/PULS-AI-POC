000100 01  W121L100.                                                            
000200*                                 LÄNKAREA TILL PROGRAMMET W12110         
000300     03 KDCALL               PIC S9(3)           COMP-3                   
000400                             VALUE ZEROS.                                 
000500*                                 ANROPSTYP                               
000600     03 LAS-UNIKROT-BENREG-ASEQ                                           
000700                             PIC S9(3)           COMP-3                   
000800                             VALUE +1.                                    
000900     03 LAS-ROT-BENREG-ASEQ  PIC S9(3)           COMP-3                   
001000                             VALUE +2.                                    
001100     03 LAS-TEXT-BENREG      PIC S9(3)           COMP-3                   
001200                             VALUE +3.                                    
001300     03 LAS-HOMONYM-BENREG   PIC S9(3)           COMP-3                   
001400                             VALUE +4.                                    
001500     03 LAS-ACTION-FILE      PIC S9(3)           COMP-3                   
001600                             VALUE +5.                                    
001700     03 REPL-ROT-BENREG      PIC S9(3)           COMP-3                   
001800                             VALUE +6.                                    
001900     03 REPL-TEXT-BENREG     PIC S9(3)           COMP-3                   
002000                             VALUE +7.                                    
002100     03 REPL-HOMONYM-BENREG  PIC S9(3)           COMP-3                   
002200                             VALUE +8.                                    
002300     03 REPL-ACTION-FILE     PIC S9(3)           COMP-3                   
002400                             VALUE +9.                                    
002500     03 ISRT-ROT-BENREG      PIC S9(3)           COMP-3                   
002600                             VALUE +10.                                   
002700     03 ISRT-TEXT-BENREG     PIC S9(3)           COMP-3                   
002800                             VALUE +11.                                   
002900     03 ISRT-HOMONYM-BENREG  PIC S9(3)           COMP-3                   
003000                             VALUE +12.                                   
003100     03 DELETA-BENREG        PIC S9(3)           COMP-3                   
003200                             VALUE +13.                                   
003300     03 LAS-UNIK-TEXT-BENREG PIC S9(3)           COMP-3                   
003400                             VALUE +14.                                   
003500     03 REPL-UNIK-TEXT-BENREG                                             
003600                             PIC S9(3)           COMP-3                   
003700                             VALUE +15.                                   
003800     03 LAS-ARTIKEL-BENREG   PIC S9(3)           COMP-3                   
003900                             VALUE +16.                                   
004000     03 REPL-ARTIKEL-BENREG  PIC S9(3)           COMP-3                   
004100                             VALUE +17.                                   
004200     03 KDSVAR               PIC X                                        
004300                             VALUE SPACE.                                 
004400*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
004500     03 KDSVAR-OK            PIC X                                        
004600                             VALUE ' '.                                   
004700*                                 SVARSKOD : OK                           
004800     03 KDSVAR-FEL           PIC X                                        
004900                             VALUE 'F'.                                   
005000*                                 SVARSKOD: FEL                           
005100*                                                                         
005200     03 IDSKYLT              PIC X(3)                                     
005300                             VALUE SPACES.                                
005400*                                 NATIONALITETSTECKEN                     
005500*                                 SPRÅKIDENTIFIKATION                     
005600     03 IDBENNR              PIC S9(7)           COMP-3                   
005700                             VALUE ZEROS.                                 
005800*                                 BENÄMNINGSNUMMER                        
005900     03 IDSEGMNR             PIC S9              COMP-3                   
006000                             VALUE ZERO.                                  
006100*                                 ORDNINGSFÖLJD PÅ SEGMENTET              
006200     03 IDARTNR              PIC S9(9)           COMP-3                   
006300                             VALUE ZEROS.                                 
006400*                                 ARTIKELNUMMER                           
006500*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
