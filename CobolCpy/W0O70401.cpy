000100 01  MOD-W0O70401.                                                        
000200*                                 COPYTEXT FÖR MOD W0O70401               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDRUTIN-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDRUTIN-UT       PIC X(8).                                    
001000*                                 RUTINNAMN (GRUPP AV JOBB)               
001100     03 MOD-IDJOB-IN         PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDJOB-UT         PIC X(8).                                    
001400*                                 JOBBNAMN                                
001500     03 MOD-IDJCLRAD-SKIP-ATTR                                            
001600                             PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-IDJCLRAD-SKIP-IN PIC X(2).                                    
001900*                                 MFS BEHANDLING AV INPUTFÄLT             
002000     03 MOD-IDJCLRAD-SKIP-UT PIC 9(5).                                    
002100*                                 RADNUMMER PÅ JCL                        
002200     03 MOD-IDJCLRAD-NEXT    PIC 9(5).                                    
002300*                                 RADNUMMER PÅ JCL                        
002400     03 MOD-BEJOB-ATTR       PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-BEJOB            PIC X(25).                                   
002700*                                 JOB BESKRIVNING                         
002800     03 MOD-IDOWNER-ATTR     PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-IDOWNER          PIC X(8).                                    
003100*                                 ÄGAREIDENTITET I RACF                   
003200     03 MOD-IDJOB-ATTR       PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-IDJOB            PIC X(8).                                    
003500*                                 JOBBNAMN                                
003600     03 MOD-KDDEBINFO        PIC X(11).                                   
003700*                                 DEBETERINGS-INFO I JOB-KORT             
003800     03 MOD-KDJROOM-ATTR     PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-KDJROOM          PIC X(4).                                    
004100*                                 ROOM I JOB-KORTET                       
004200     03 MOD-BEPGMNAMN-ATTR   PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-BEPGMNAMN        PIC X(20).                                   
004500*                                 PROGRAMMER'S NAME IN JOBCARD            
004600     03 MOD-KDMSGCLASS-ATTR  PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-KDMSGCLASS       PIC X.                                       
004900*                                 MSGCLASS I JOB-KORTET                   
005000     03 MOD-KDMSGLEVEL-ATTR  PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-KDMSGLEVEL       PIC X(3).                                    
005300*                                 MSGLEVEL I JOB-KORTET                   
005400     03 MOD-KDJCLASS-ATTR    PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-KDJCLASS         PIC X.                                       
005700*                                 JOB CLASS I JOB-KORTET                  
005800     03 MOD-IDNOTIFY-ATTR    PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-IDNOTIFY         PIC X(8).                                    
006100*                                 NOTIFY I JOB-KORTET                     
006200     03 MOD-KVJTIME-ATTR     PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-KVJTIME          PIC 9(4).                                    
006500*                                 TIME PARAMETER I JOB-KORTET             
006600     03 MOD-KVJLINES-ATTR    PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-KVJLINES         PIC 9(4).                                    
006900*                                 ANTAL RADER I JOB-KORTET                
007000     03 MOD-KDJFORMS-ATTR    PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-KDJFORMS         PIC X(4).                                    
007300*                                 FORMS NUMBER IN THE JOB-CARD            
007400     03 MOD-KVJCOUNT-ATTR    PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600     03 MOD-KVJCOUNT         PIC X(3).                                    
007700*                                 ANTAL RADER PER SIDA I SYSOUT           
007800     03 MOD-KDROUTEX-ATTR    PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-KDROUTEX         PIC X(8).                                    
008100*                                 ROUTE XEQ                               
008200     03 MOD-KDROUTEP-ATTR    PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400     03 MOD-KDROUTEP         PIC X(8).                                    
008500*                                 ROUTE PRINT                             
008600     03 MOD-IDPROCDD-ATTR    PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800     03 MOD-IDPROCDD         PIC X(8).                                    
008900*                                 PROCLIB DDNAME IN JOBPARM CARD          
009000     03 MOD-KDOUTPUT-ATTR    PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200     03 MOD-KDOUTPUT         PIC X(29).                                   
009300*                                 JES2 OUTPUT                             
009400     03 MOD-KDTRSTAT-IN-ATTR PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600     03 MOD-KDTRSTAT-IN      PIC X(2).                                    
009700*                                 MFS BEHANDLING AV INPUTFÄLT             
009800     03 MOD-KDTRSTAT         PIC 9.                                       
009900*                                 TRANSAKTIONSSTATUS                      
010000     03 MOD-LINES            OCCURS 8 TIMES                               
010100                             INDEXED MOD-IX-LINE.                         
010200        05 MOD-IDJCLRAD-ATTR PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400        05 MOD-IDJCLRAD      PIC 9(5).                                    
010500*                                 RADNUMMER PÅ JCL                        
010600        05 MOD-TEJCL-ATTR    PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800        05 MOD-TEJCL         PIC X(71).                                   
010900*                                 JCL-KORT                                
011000     03 MOD-TEMFSINF         PIC X(61).                                   
011100*                                 INFORMATIONSMEDDELANDE                  
011200*** END COPY W0O70401    LENGTH=975                                       
