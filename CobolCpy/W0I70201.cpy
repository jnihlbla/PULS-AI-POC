000100 01  MID-W0I70201.                                                        
000200*                                 COPYTEXT FÖR MID W0I70201               
000300     03 MID-IDRUTIN-IN       PIC X(8).                                    
000400*                                 RUTINNAMN (GRUPP AV JOBB)               
000500     03 MID-IDRUTIN-UT       PIC X(8).                                    
000600*                                 RUTINNAMN (GRUPP AV JOBB)               
000700     03 MID-IDJOB-IN         PIC X(8).                                    
000800*                                 JOBBNAMN                                
000900     03 MID-IDJOB-UT         PIC X(8).                                    
001000*                                 JOBBNAMN                                
001100     03 MID-IDJCLRAD-SKIP-IN PIC 9(5).                                    
001200*                                 RADNUMMER PÅ JCL                        
001300     03 MID-IDJCLRAD-SKIP-UT PIC 9(5).                                    
001400*                                 RADNUMMER PÅ JCL                        
001500     03 MID-IDJCLRAD-SPAR    PIC 9(5).                                    
001600*                                 RADNUMMER PÅ JCL                        
001700     03 MID-BERUTIN          PIC X(25).                                   
001800*                                 RUTIN BESKRIVNING                       
001900     03 MID-IDOWNER          PIC X(8).                                    
002000*                                 ÄGAREIDENTITET I RACF                   
002100     03 MID-IDJOB            PIC X(8).                                    
002200*                                 JOBBNAMN                                
002300     03 MID-KDDEBINFO        PIC X(11).                                   
002400*                                 DEBETERINGS-INFO I JOB-KORT             
002500     03 MID-KDJROOM          PIC X(4).                                    
002600*                                 ROOM I JOB-KORTET                       
002700     03 MID-BEPGMNAMN        PIC X(20).                                   
002800*                                 PROGRAMMER'S NAME IN JOBCARD            
002900     03 MID-KDMSGCLASS       PIC X.                                       
003000*                                 MSGCLASS I JOB-KORTET                   
003100     03 MID-KDMSGLEVEL       PIC X(3).                                    
003200*                                 MSGLEVEL I JOB-KORTET                   
003300     03 MID-KDJCLASS         PIC X.                                       
003400*                                 JOB CLASS I JOB-KORTET                  
003500     03 MID-KVJTIME          PIC 9(4).                                    
003600*                                 TIME PARAMETER I JOB-KORTET             
003700     03 MID-KVJLINES         PIC 9(4).                                    
003800*                                 ANTAL RADER I JOB-KORTET                
003900     03 MID-KDJFORMS         PIC X(4).                                    
004000*                                 FORMS NUMBER IN THE JOB-CARD            
004100     03 MID-KDROUTEX         PIC X(8).                                    
004200*                                 ROUTE XEQ                               
004300     03 MID-KDROUTEP         PIC X(8).                                    
004400*                                 ROUTE PRINT                             
004500     03 MID-IDPROCDD         PIC X(8).                                    
004600*                                 PROCLIB DDNAME IN JOBPARM CARD          
004700     03 MID-KDOUTPUT         PIC X(29).                                   
004800*                                 JES2 OUTPUT                             
004900     03 MID-LINES            OCCURS 4 TIMES                               
005000                             INDEXED MID-IX-LINE.                         
005100        05 MID-IDJCLRAD      PIC 9(5).                                    
005200*                                 RADNUMMER PÅ JCL                        
005300        05 MID-TEJCL         PIC X(71).                                   
005400*                                 JCL-KORT                                
005500*** END COPY W0I70201C0  LENGTH=497                                       
