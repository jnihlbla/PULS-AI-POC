000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.  W9702700.                                                   
000400     SKIP2                                                                
000500 AUTHOR.        KARIN OLSSON.                                             
000600     DATE-WRITTEN.  MAY 1993.                                             
000700*                                                                         
000800*                                                                         
000900*    PLOCKAR FRAM ALLA TRANSAR SOM INTE HAR NÅGRA ANVÄNDARE.              
001000*                                                                         
001100     EJECT                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP2                                                                
001400 INPUT-OUTPUT  SECTION.                                                   
001500*                                                                         
001600 FILE-CONTROL.                                                            
001700*                                                                         
001800     SELECT MEMFIL          ASSIGN TO W97027D1.                           
001900*                                                                         
001910     SELECT REGELFIL        ASSIGN TO W97027D2.                           
001920*                                                                         
002000     SELECT BESKRFIL        ASSIGN TO W97027D3.                           
002100*                                                                         
002400     SELECT UTFIL           ASSIGN TO W97027D4.                           
002500*                                                                         
002600*                                                                         
002700 DATA DIVISION.                                                           
002800                                                                          
002900 FILE  SECTION.                                                           
003000*                                                                         
003100 FD  MEMFIL                                                               
003200     LABEL RECORD   STANDARD                                              
003300     RECORDING      F                                                     
003400     BLOCK CONTAINS 0.                                                    
003500                                                                          
003600 01  FILLER                  PIC X(80).                                   
003700*                                                                         
003710 FD  REGELFIL                                                             
003720     LABEL RECORD   STANDARD                                              
003730     RECORDING      F                                                     
003740     BLOCK CONTAINS 0.                                                    
003750                                                                          
003760 01  FILLER                  PIC X(60).                                   
003770*                                                                         
003800 FD  BESKRFIL                                                             
003900     LABEL RECORD   STANDARD                                              
004000     RECORDING      F                                                     
004100     BLOCK CONTAINS 0.                                                    
004200                                                                          
004300 01  FILLER                  PIC X(133).                                  
004400*                                                                         
005200 FD  UTFIL                                                                
005300     LABEL RECORD   STANDARD                                              
005400     RECORDING      F                                                     
005500     BLOCK CONTAINS 0.                                                    
005600                                                                          
005700 01  UT-POST                 PIC X(60).                                   
005800     EJECT                                                                
005900 WORKING-STORAGE  SECTION.                                                
005901                                                                          
005910*    -- CHECKED BY WY2000                                                 
006000*                                                                         
006100 01  W-MEM-AREA.                                                          
006200     03  W-MEM-TRANS             PIC X(8).                                
006210     03  FILLER                  PIC X(72).                               
006300*                                                                         
006310 01  W-REGEL-AREA.                                                        
006320     03  W-REGEL-TRANS           PIC X(8).                                
006321     03  FILLER                  PIC X(52).                               
006330*                                                                         
008200 01  W-BESKR-AREA.                                                        
008300     03  FILLER                  PIC X(37).                               
008400     03  W-BESKR-TRANS           PIC X(8).                                
008500     03  FILLER                  PIC X(17).                               
008600     03  W-BESKR-TEXT            PIC X(30).                               
008700     03  FILLER                  PIC X(41).                               
008800*                                                                         
008900 01  W-BESKR-AREA-2 REDEFINES W-BESKR-AREA.                               
009000     03  FILLER                  PIC X(37).                               
009100     03  W-TRANSACTION           PIC X(11).                               
009200     03  FILLER                  PIC X(3).                                
009300     03  W-ACCOUNT               PIC X(7).                                
009400     03  FILLER                  PIC X(15).                               
009500     03  W-COMMENTS              PIC X(8).                                
009600     03  FILLER                  PIC X(52).                               
009700*                                                                         
012100 01  UT-AREA.                                                             
012200*                                                                         
012300     03  UT-TRANS            PIC X(8)   VALUE SPACE.                      
012400     03  UT-BESKR            PIC X(30)  VALUE SPACE.                      
012500     03  UT-BOLAG            PIC X(3)   VALUE SPACE.                      
012600     03  UT-LAND             PIC X(2)   VALUE SPACE.                      
012700     03  UT-JOBFUNC          PIC X(2)   VALUE SPACE.                      
012800     03  UT-DIVMISC          PIC X(3)   VALUE SPACE.                      
013000     03  FILLER              PIC X(12)  VALUE SPACE.                      
013100                                                                          
013200 01  TRANSACTION-TXT         PIC X(11) VALUE 'TRANSACTION'.               
013300 01  ACCOUNT-TXT             PIC X(7)  VALUE 'ACCOUNT'.                   
013400 01  COMMENTS-TXT            PIC X(8)  VALUE 'COMMENTS'.                  
014200*                                                                         
014300 01  GENERELLA-SUBPGM.                                                    
014400*                                                                         
014500     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
014700     EJECT                                                                
014800* --- PARAMETRAR TILL ABEND                                               
014900 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   COMP VALUE +16.                  
015000 01  RKOD-ABEND-MED-DUMP     PIC S9(4)   COMP VALUE +1000.                
015100*                                                                         
017000 01  GENERELLA-KONSTANTER.                                                
017100*                                                                         
017200     03  JA                  PIC X(1)    VALUE 'Y'.                       
017300     03  NEJ                 PIC X(1)    VALUE 'N'.                       
017600                                                                          
017610 01  MEMFIL-EOF              PIC X(1)    VALUE 'N'.                       
017700 01  REGELFIL-EOF            PIC X(1)    VALUE 'N'.                       
017800 01  BESKRFIL-EOF            PIC X(1)    VALUE 'N'.                       
018000*                                                                         
018700     SKIP2                                                                
018800 01  RETURKODER.                                                          
018900*                                                                         
019000     03  RKOD                PIC S9(4)   COMP SYNC VALUE ZERO.            
019100     EJECT                                                                
019200 PROCEDURE DIVISION.                                                      
019300*                                                                         
019400     PERFORM A-INIT                                                       
019500     PERFORM S10-LAES-MEMFIL                                              
019510     PERFORM S11-LAES-REGELFIL                                            
019520     PERFORM S12-LAES-BESKRFIL                                            
019600     PERFORM B-LAES-FRAM-BESKRFIL                                         
019900     PERFORM UNTIL MEMFIL-EOF = JA                                        
020200       IF W-MEM-TRANS = W-REGEL-TRANS                                     
020210         PERFORM S10-LAES-MEMFIL                                          
020220         PERFORM S11-LAES-REGELFIL                                        
020230       ELSE                                                               
020240         IF W-MEM-TRANS < W-REGEL-TRANS                                   
020250           IF W-MEM-TRANS (3:1) = 'T'                                     
020260             IF W-MEM-TRANS = W-BESKR-TRANS                               
020270               MOVE W-MEM-TRANS TO UT-TRANS                               
020280               MOVE W-BESKR-TEXT TO UT-BESKR                              
020290               MOVE 'V4N' TO UT-BOLAG                                     
020291               MOVE 'OA'  TO UT-LAND                                      
020292               MOVE 'NV'  TO UT-JOBFUNC                                   
020293               PERFORM S20-SKRIV-UTFIL                                    
020294               PERFORM S10-LAES-MEMFIL                                    
020296               PERFORM S12-LAES-BESKRFIL                                  
020700             ELSE                                                         
020800               IF (W-TRANSACTION = TRANSACTION-TXT AND                    
020900                  W-ACCOUNT      = ACCOUNT-TXT AND                        
021000                  W-COMMENTS     = COMMENTS-TXT) OR                       
021100                  (W-TRANSACTION  = ALL '-' OR SPACES)                    
021200                 PERFORM S12-LAES-BESKRFIL                                
021300               ELSE                                                       
021400                 IF W-MEM-TRANS < W-BESKR-TRANS                           
021500                   MOVE W-MEM-TRANS TO UT-TRANS                           
021600                   MOVE 'okänd trans' TO UT-BESKR                         
021700                   MOVE 'V4N' TO UT-BOLAG                                 
021800                   MOVE 'OA'  TO UT-LAND                                  
021900                   MOVE 'NV'  TO UT-JOBFUNC                               
022000                   PERFORM S20-SKRIV-UTFIL                                
022100                   PERFORM S10-LAES-MEMFIL                                
022500                 ELSE                                                     
022501                   PERFORM S12-LAES-BESKRFIL                              
022700                 END-IF                                                   
022800               END-IF                                                     
022900             END-IF                                                       
023000           ELSE                                                           
023010             PERFORM S10-LAES-MEMFIL                                      
023011           END-IF                                                         
023020         ELSE                                                             
023030           PERFORM S11-LAES-REGELFIL                                      
026200         END-IF                                                           
026800       END-IF                                                             
026900     END-PERFORM                                                          
027000                                                                          
027100     PERFORM Z-FINIT                                                      
027200     MOVE RKOD TO RETURN-CODE                                             
027300     GOBACK.                                                              
027400     EJECT                                                                
027500 A-INIT  SECTION.                                                         
027600     SKIP2                                                                
027700     OPEN INPUT MEMFIL REGELFIL BESKRFIL                                  
027800                                                                          
027900     OPEN OUTPUT UTFIL                                                    
028000     MOVE NEJ TO MEMFIL-EOF REGELFIL-EOF BESKRFIL-EOF                     
028100     MOVE SPACE TO UT-AREA                                                
028200     .                                                                    
028300     EJECT                                                                
028400 B-LAES-FRAM-BESKRFIL  SECTION.                                           
028500     SKIP2                                                                
028600     PERFORM S12-LAES-BESKRFIL                                            
028700     PERFORM UNTIL BESKRFIL-EOF = JA                                      
028800              OR (W-TRANSACTION = TRANSACTION-TXT                         
028900                AND W-ACCOUNT = ACCOUNT-TXT                               
029000                AND W-COMMENTS = COMMENTS-TXT)                            
029100       PERFORM S12-LAES-BESKRFIL                                          
029200     END-PERFORM                                                          
029300     .                                                                    
029400     EJECT                                                                
044700 Z-FINIT  SECTION.                                                        
044800     SKIP2                                                                
044900     CLOSE MEMFIL                                                         
044910           REGELFIL                                                       
045000           BESKRFIL                                                       
045200           UTFIL                                                          
045300     .                                                                    
045400 S10-LAES-MEMFIL SECTION.                                                 
045500     SKIP2                                                                
045600     READ MEMFIL INTO W-MEM-AREA                                          
045700       AT END MOVE JA TO MEMFIL-EOF                                       
045800     .                                                                    
045810 S11-LAES-REGELFIL SECTION.                                               
045820     SKIP2                                                                
045830     READ REGELFIL INTO W-REGEL-AREA                                      
045841       AT END                                                             
045842         MOVE JA TO REGELFIL-EOF                                          
045843         MOVE '99999999' TO W-REGEL-TRANS                                 
045844       END-READ                                                           
045850     .                                                                    
045900 S12-LAES-BESKRFIL SECTION.                                               
046000     SKIP2                                                                
046100     READ BESKRFIL INTO W-BESKR-AREA                                      
046200       AT END                                                             
046300         MOVE JA TO BESKRFIL-EOF                                          
046400         MOVE '99999999' TO W-BESKR-TRANS                                 
046500       END-READ                                                           
046600     .                                                                    
047500 S20-SKRIV-UTFIL  SECTION.                                                
047600     SKIP2                                                                
047700     WRITE UT-POST   FROM UT-AREA                                         
047800     .                                                                    
047900 S99-ABEND  SECTION.                                                      
048000     SKIP2                                                                
048100     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
048200     .                                                                    
