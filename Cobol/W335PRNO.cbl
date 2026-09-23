000010*COMPOPT STDSUB=YES                                                       
000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W335PRNO.                                                
000400 AUTHOR.         CHRISTINE LINDQVIST                                      
000500 DATE-WRITTEN.   MARS -02.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*       -PROGRAMMET TAR UT NÄSTA LEDIGA PRISFRÅGENUMMER                   
001100*        DESSA ANVÄNDS FÖR ATT KUNNA UPPDATERA PRISFRÅGOR                 
001200*        PÅ WDC7.                                                         
002100*                                                                         
002110*       -KDCALL 1 = HÄMTA LEDIGT PRISFRÅGENUMMER                          
002120*                                                                         
002130*       -KDCALL 2 = LÄGGA TILL +1 TILL PRISFRÅGENR                        
002140*                                                                         
002150*       -KDCALL 3 = UPPDATERA NÄSTA LEDIGA PRISFRÅGENR                    
002160*                                                                         
002191*                                                                         
002200*        PROGRAMMET LÄSER      WDR1                                       
002500*                                                                         
002600*    LÄNKAREA: W335PRNO                                                   
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100     SKIP3                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003301                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W335PRNO'.            
003800                                                                          
003900 01  GENERELLA-SUBPROGRAM.                                                
004000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004300     EJECT                                                                
004400 01  FELTEXT.                                                             
004410     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004420     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
004600*                                                                         
004700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
004800     SKIP3                                                                
004900*    --- STATUS-KOD FRÅN IMS                                              
005403     SKIP2                                                                
005410 01  STATUS-WS                   PIC XX.                                  
005420     88  SEGMENT-FINNS                       VALUE '  '.                  
005430     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
005440     SKIP2                                                                
005500 01  GODK-STATUSKODER.                                                    
005600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
005700     SKIP3                                                                
005800 01  SSA1                        PIC X(128).                              
005900 01  SSA2                        PIC X(64).                               
006000     EJECT                                                                
006100*    --- IMS FUNKTIONSKODER                                               
006200*01  -COPY W0003                                                          
006400     EJECT                                                                
006500 01  NYCKLAR-TILL-DLI.                                                    
006600     03  W-WDGXKEY-3107-X.                                                
006700         05  W-IDHTYP            PIC X(4)    VALUE '3107'.                
006800         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
007300                                                                          
008600     EJECT                                                                
008800*    ---  DLI INPUT-OUTPUT AREA                                           
009500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3108'.                    
009600 01  DLI-IO-WDGX3108.                                                     
009700*    03  -COPY WDGX3108                                                   
009800     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500*                                                                         
010600*   -COPY W335PRNO                                                        
010900     EJECT                                                                
011000*01  -COPY W0008      -PRE 3107-                                          
011200     05  FILLER                  PIC X.                                   
011300     SKIP2                                                                
012140     EJECT                                                                
012200 PROCEDURE DIVISION  USING PRNO-W335PRNO 3107-PCB.                        
012800                                                                          
012810     IF PRNO-KDCALL = 1                                                   
012820        PERFORM A-HAMTA-PRISFRAGENR                                       
012830     ELSE                                                                 
012900        IF PRNO-KDCALL = 2                                                
013000           PERFORM B-ADDERA-1-TILL-PRISFRAGENR                            
013110        ELSE                                                              
013120          IF PRNO-KDCALL = 3                                              
013121            PERFORM C-UPPDATERA-NASTA-PRISFRAGENR                         
013130          END-IF                                                          
013200        END-IF                                                            
013300     END-IF                                                               
015000     GOBACK                                                               
015100     .                                                                    
015200     EJECT                                                                
015300 A-HAMTA-PRISFRAGENR   SECTION.                                           
015400                                                                          
015410     PERFORM IMS-GHU-WDGX3108                                             
015500     MOVE 3108-IDPRQUES-NEXT TO PRNO-IDPRQUES-UT                          
016400     .                                                                    
016500     EJECT                                                                
016501 B-ADDERA-1-TILL-PRISFRAGENR    SECTION.                                  
016502                                                                          
016503     ADD +1 TO PRNO-IDPRQUES-IN                                           
016504     IF PRNO-IDPRQUES-IN = +999999                                        
016505       MOVE +1 TO PRNO-IDPRQUES-IN                                        
016506     END-IF                                                               
016507     MOVE PRNO-IDPRQUES-IN TO PRNO-IDPRQUES-UT                            
016508     .                                                                    
016509                                                                          
016510                                                                          
016511 C-UPPDATERA-NASTA-PRISFRAGENR  SECTION.                                  
016531                                                                          
016532     ADD +1 TO PRNO-IDPRQUES-IN                                           
016540     IF PRNO-IDPRQUES-IN = +999999                                        
016550       MOVE +1 TO PRNO-IDPRQUES-IN                                        
016560     END-IF                                                               
016561                                                                          
016562     MOVE PRNO-IDPRQUES-IN TO 3108-IDPRQUES-NEXT                          
016563     PERFORM IMS-REPL-WDGX3108                                            
016593     .                                                                    
016594     EJECT                                                                
023401                                                                          
023500******IMS-SEKTIONER*******************************                        
023600     SKIP2                                                                
023700 IMS-GHU-WDGX3108      SECTION.                                           
023800                                                                          
023900     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-3107-X ')'                    
024000            DELIMITED BY SIZE INTO SSA1                                   
024100     MOVE 'WDGX3108'            TO SSA2                                   
024300     MOVE '  ' TO GODK-STATUSKODER                                        
024400     CALL CBLTDLI USING GHU 3107-PCB DLI-IO-WDGX3108 SSA1 SSA2            
024500     MOVE 3107-STATUS-CODE TO STATUS-WS                                   
024600     PERFORM IMS-STATUSKONTROLL                                           
024700     .                                                                    
024710     SKIP2                                                                
024900 IMS-REPL-WDGX3108 SECTION.                                               
025000                                                                          
025100     MOVE '  ' TO GODK-STATUSKODER                                        
025200     CALL CBLTDLI USING REPL 3107-PCB DLI-IO-WDGX3108.                    
025300     MOVE 3107-STATUS-CODE TO STATUS-WS                                   
025400     PERFORM IMS-STATUSKONTROLL                                           
025500     .                                                                    
025600     SKIP2                                                                
028500 IMS-STATUSKONTROLL SECTION.                                              
028600                                                                          
028700     SET STATUS-IX TO 1                                                   
028800     SEARCH GODK-STATUS                                                   
028900       AT END                                                             
029000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
029100           DELIMITED BY SIZE INTO FELTEXT                                 
029200         DISPLAY FELTEXT                                                  
029300         CALL FELLOG                                                      
029400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
029500         CONTINUE                                                         
029600     END-SEARCH                                                           
029700     .                                                                    
