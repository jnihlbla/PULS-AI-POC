000100 01  MID-W0I70401.                                                        
000200*                                 COPYTEXT FÖR MID W0I70401               
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
001500     03 MID-IDJCLRAD-NEXT    PIC 9(5).                                    
001600*                                 RADNUMMER PÅ JCL                        
001700     03 MID-BEJOB            PIC X(25).                                   
001800*                                 JOB BESKRIVNING                         
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
003500     03 MID-IDNOTIFY         PIC X(8).                                    
003600*                                 NOTIFY I JOB-KORTET                     
003700     03 MID-KVJTIME          PIC 9(4).                                    
003800*                                 TIME PARAMETER I JOB-KORTET             
003900     03 MID-KVJLINES         PIC 9(4).                                    
004000*                                 ANTAL RADER I JOB-KORTET                
004100     03 MID-KDJFORMS         PIC X(4).                                    
004200*                                 FORMS NUMBER IN THE JOB-CARD            
004300     03 MID-KVJCOUNT         PIC X(3).                                    
004400*                                 ANTAL RADER PER SIDA I SYSOUT           
004500     03 MID-KDROUTEX         PIC X(8).                                    
004600*                                 ROUTE XEQ                               
004700     03 MID-KDROUTEP         PIC X(8).                                    
004800*                                 ROUTE PRINT                             
004900     03 MID-IDPROCDD         PIC X(8).                                    
005000*                                 PROCLIB DDNAME IN JOBPARM CARD          
005100     03 MID-KDOUTPUT         PIC X(29).                                   
005200*                                 JES2 OUTPUT                             
005300     03 MID-KDTRSTAT         PIC 9.                                       
005400*                                 TRANSAKTIONSSTATUS                      
005500     03 MID-LINES            OCCURS 8 TIMES                               
005600                             INDEXED MID-IX-LINE.                         
005700        05 MID-IDJCLRAD      PIC 9(5).                                    
005800*                                 RADNUMMER PÅ JCL                        
005900        05 MID-TEJCL         PIC X(71).                                   
006000*                                 JCL-KORT                                
006100*** END COPY W0I70401    LENGTH=813                                       
