000100 01  MOD-W0O70201.                                                        
000200*                                 COPYTEXT FÖR MOD W0O70201               
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
002200     03 MOD-IDJCLRAD-SPAR    PIC 9(5).                                    
002300*                                 RADNUMMER PÅ JCL                        
002400     03 MOD-BERUTIN-ATTR     PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-BERUTIN          PIC X(25).                                   
002700*                                 RUTIN BESKRIVNING                       
002800     03 MOD-IDOWNER-ATTR     PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-IDOWNER          PIC X(8).                                    
003100*                                 ÄGAREIDENTITET I RACF                   
003200     03 MOD-IDJOB-ATTR       PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-IDJOB            PIC X(8).                                    
003500*                                 JOBBNAMN                                
003600     03 MOD-KDDEBINFO-ATTR   PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-KDDEBINFO        PIC X(11).                                   
003900*                                 DEBETERINGS-INFO I JOB-KORT             
004000     03 MOD-KDJROOM-ATTR     PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-KDJROOM          PIC X(4).                                    
004300*                                 ROOM I JOB-KORTET                       
004400     03 MOD-BEPGMNAMN-ATTR   PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-BEPGMNAMN        PIC X(20).                                   
004700*                                 PROGRAMMER'S NAME IN JOBCARD            
004800     03 MOD-KDMSGCLASS-ATTR  PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-KDMSGCLASS       PIC X.                                       
005100*                                 MSGCLASS I JOB-KORTET                   
005200     03 MOD-KDMSGLEVEL-ATTR  PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-KDMSGLEVEL       PIC X(3).                                    
005500*                                 MSGLEVEL I JOB-KORTET                   
005600     03 MOD-KDJCLASS-ATTR    PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-KDJCLASS         PIC X.                                       
005900*                                 JOB CLASS I JOB-KORTET                  
006000     03 MOD-KVJTIME-ATTR     PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-KVJTIME          PIC 9(4).                                    
006300*                                 TIME PARAMETER I JOB-KORTET             
006400     03 MOD-KVJLINES-ATTR    PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 MOD-KVJLINES         PIC 9(4).                                    
006700*                                 ANTAL RADER I JOB-KORTET                
006800     03 MOD-KDJFORMS-ATTR    PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-KDJFORMS         PIC X(4).                                    
007100*                                 FORMS NUMBER IN THE JOB-CARD            
007200     03 MOD-KDROUTEX-ATTR    PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-KDROUTEX         PIC X(8).                                    
007500*                                 ROUTE XEQ                               
007600     03 MOD-KDROUTEP-ATTR    PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-KDROUTEP         PIC X(8).                                    
007900*                                 ROUTE PRINT                             
008000     03 MOD-IDPROCDD-ATTR    PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 MOD-IDPROCDD         PIC X(8).                                    
008300*                                 PROCLIB DDNAME IN JOBPARM CARD          
008400     03 MOD-KDOUTPUT-ATTR    PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600     03 MOD-KDOUTPUT         PIC X(29).                                   
008700*                                 JES2 OUTPUT                             
008800     03 MOD-LINES            OCCURS 4 TIMES                               
008900                             INDEXED MOD-IX-LINE.                         
009000        05 MOD-IDJCLRAD-ATTR PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200        05 MOD-IDJCLRAD      PIC 9(5).                                    
009300*                                 RADNUMMER PÅ JCL                        
009400        05 MOD-TEJCL-ATTR    PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600        05 MOD-TEJCL         PIC X(71).                                   
009700*                                 JCL-KORT                                
009800     03 MOD-TEMFSINF         PIC X(61).                                   
009900*                                 INFORMATIONSMEDDELANDE                  
010000*** END COPY W0O70201C0  LENGTH=637                                       
