000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6118200.                                                
000400*AUTHOR.         BERT ANDERSSON.                                          
000500*DATE-WRITTEN.   92/04/29.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET ÄR EN BMP SOM LÄSER FIL W61181 FRÅN                   
001100*        PGM. W6118100 OCH RENSAR DE FÖLJESEDLAR FRÅN W6D1                
001200*        SOM FINNS PÅ FILEN.                                              
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001500*                                                                         
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- IN-FIL W61181                                              
002900     SELECT W61181                     ASSIGN TO W61182D1.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W61181                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800     SKIP2                                                                
003900*01  -COPY W6118101    -L.                                                
004000     SKIP2                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200     SKIP2                                                                
004201                                                                          
004210*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'W6118200'.            
004400 01  CHKP-VAR.                                                            
004500 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004600 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004700 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004800 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004900 03  ANTAL-LAESTA-POSTER         PIC S9(3)   VALUE +0.                    
005000 03  MAX-ANTAL-UPPDAT            PIC S9(3)   VALUE +15.                   
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300     SKIP2                                                                
005400 01  FELTEXT.                                                             
005500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005700                                                                          
005800 77  W61181-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W61181                       VALUE 'J'.                   
005901*                                                                         
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006500     EJECT                                                                
006600*    --- PARAMETRAR TILL POSTSUM                                          
006700*                                                                         
006800*01  -COPY W0005   -PRE  POSTSUM-                                         
006900     EJECT                                                                
007000 01  IN-AREA-START               PIC X(24)   VALUE                        
007100                                             'IN-AREA-START'.             
007200     SKIP2                                                                
007300                                                                          
007400*01  AREA -COPY W6118101   -PRE IN-                                       
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008000     03  W-W6D101KY-X.                                                    
008200         05  W-IDDC              PIC  XX     VALUE SPACE.                 
008210         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
008300         05  W-IDFS              PIC  X(8).                               
008400         05  W-TIAVIDAT          PIC S9(7)   VALUE ZERO COMP-3.           
008500     SKIP2                                                                
008600*    --- STATUS-KOD FRÅN IMS                                              
008700 01  STATUS-WS                   PIC XX.                                  
008800     88  SEGMENT-FINNS                       VALUE '  '.                  
008900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009200     88  IMS-EJ-OK                           VALUE 'XD'.                  
009300     SKIP2                                                                
009400 01  GODK-STATUSKODER.                                                    
009500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009600     SKIP3                                                                
009700 01  SSA1                        PIC X(64).                               
009800 01  SSA2                        PIC X(64).                               
009900     EJECT                                                                
010000*    --- IMS FUNKTIONSKODER                                               
010100*01  -COPY W0003                                                          
010200     EJECT                                                                
010300*    ---  DLI INPUT-OUTPUT AREA                                           
010400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010500     SKIP3                                                                
010600 01  DLI-IO-AREA.                                                         
010700     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
010800     SKIP3                                                                
010900     03  W6INLA01 REDEFINES IO-AREA.                                      
011000*        05  -COPY W6D101  -PRE INLA-                                     
011700 LINKAGE SECTION.                                                         
011800                                                                          
011900*01  -COPY W0009   -PRE MSG-                                              
012000     SKIP2                                                                
012100*01  -COPY W0008  -PRE INLA-                                              
012200     05  FILLER                  PIC X.                                   
012201     SKIP2                                                                
012210*01  -COPY W0008  -PRE CKPB-                                              
012220     05  FILLER                  PIC X.                                   
012300     EJECT                                                                
012400 PROCEDURE DIVISION  USING MSG-PCB INLA-PCB CKPB-PCB.                     
012500     ENTRY 'DLITCBL' USING MSG-PCB INLA-PCB CKPB-PCB.                     
012600                                                                          
012700     PERFORM A-INIT                                                       
012800     PERFORM S01-LAES-W61181                                              
012900     PERFORM UNTIL END-OF-W61181                                          
013000                                                                          
013100       PERFORM B-EV-DLET-AV-INLA01                                        
013200                                                                          
013300       IF ANTAL-LAESTA-POSTER > MAX-ANTAL-UPPDAT                          
013400         PERFORM X-TAG-CHECKPOINT                                         
013500       END-IF                                                             
013600                                                                          
013700       PERFORM S01-LAES-W61181                                            
013800     END-PERFORM                                                          
013900                                                                          
014000                                                                          
014100     PERFORM Z-FINIT                                                      
014200                                                                          
014300     MOVE ZERO TO RETURN-CODE                                             
014400     GOBACK                                                               
014500     .                                                                    
014600     EJECT                                                                
014700 A-INIT SECTION.                                                          
014800                                                                          
014900     OPEN INPUT W61181                                                    
015000                                                                          
015100     MOVE IDPGM                     TO POSTSUM-PROGNAMN                   
015110     MOVE ZERO                      TO ANTAL-LAESTA-POSTER                
015200                                                                          
015300     PERFORM IMS-RESTART                                                  
016200     .                                                                    
016300     SKIP2                                                                
017300 B-EV-DLET-AV-INLA01  SECTION.                                            
017400                                                                          
017500     PERFORM IMS-GHU-INLA01                                               
017600     IF SEGMENT-FINNS                                                     
017700       PERFORM IMS-DLET-INLA                                              
017800     END-IF                                                               
017900     .                                                                    
018000     SKIP2                                                                
018100 Z-FINIT SECTION.                                                         
018200                                                                          
019400     CLOSE W61181                                                         
019500                                                                          
019600     MOVE 'S' TO POSTSUM-OPKOD                                            
019700                                                                          
019800     CALL POSTSUM USING POSTSUM-PARM                                      
020000     .                                                                    
020100     SKIP2                                                                
020200 S01-LAES-W61181  SECTION.                                                
020300                                                                          
020400     READ W61181                    INTO IN-AREA                          
020500     AT END                                                               
020700        SET END-OF-W61181           TO TRUE                               
020800                                                                          
020900     NOT AT END                                                           
021000        MOVE IN-IDDC                TO W-IDDC                             
021100        MOVE IN-IDLEVNR             TO W-IDLEVNR                          
021200        MOVE IN-IDFS                TO W-IDFS                             
021300        MOVE IN-TIAVIDAT            TO W-TIAVIDAT                         
021301                                                                          
021400        MOVE 'W61181'               TO POSTSUM-FDNAMN                     
021500        MOVE 'W61182D1'             TO POSTSUM-DDNAMN2                    
021700        CALL POSTSUM USING POSTSUM-PARM                                   
021800                                                                          
021900        ADD 1                       TO ANTAL-LAESTA-POSTER                
022000     END-READ                                                             
022100     .                                                                    
022200     SKIP2                                                                
022300 X-TAG-CHECKPOINT   SECTION.                                              
022400                                                                          
022500* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
022600* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
022700* --- LÄS OM DATABAS OM DET BEHÖVS                                        
024300     PERFORM IMS-CHECKPOINT                                               
024400     MOVE ZERO                      TO ANTAL-LAESTA-POSTER                
024500     .                                                                    
024600     EJECT                                                                
024700* --- IMS SEKTIONER ---                                                   
024800     SKIP3                                                                
024900 IMS-GHU-INLA01 SECTION.                                                  
025000     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
025100          DELIMITED BY SIZE INTO SSA1                                     
025200     MOVE '  GE' TO GODK-STATUSKODER                                      
025300     CALL CBLTDLI USING GHU INLA-PCB DLI-IO-AREA SSA1                     
025400     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
025500     PERFORM IMS-STATUSKONTROLL                                           
025600     .                                                                    
025700     SKIP3                                                                
025800 IMS-DLET-INLA SECTION.                                                   
025900                                                                          
026000     MOVE '  ' TO GODK-STATUSKODER                                        
026100     CALL CBLTDLI USING DLET INLA-PCB DLI-IO-AREA                         
026200     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
026300     PERFORM IMS-STATUSKONTROLL                                           
026400     .                                                                    
026500     SKIP2                                                                
026600 IMS-RESTART SECTION.                                                     
026700     SKIP2                                                                
026800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026900     MOVE '  ' TO GODK-STATUSKODER                                        
027000     CALL CBLTDLI USING XRST MSG-PCB                                      
027100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027200                        CHKP-AREA-LENGTH CHKP-AREA                        
027300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027400     PERFORM IMS-STATUSKONTROLL                                           
027500     .                                                                    
027600     SKIP2                                                                
027700 IMS-CHECKPOINT SECTION.                                                  
027800     SKIP2                                                                
027900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
028000     MOVE '  XD' TO GODK-STATUSKODER                                      
028100     CALL CBLTDLI USING CHKP MSG-PCB                                      
028200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
028300                        CHKP-AREA-LENGTH CHKP-AREA                        
028400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028500     PERFORM IMS-STATUSKONTROLL                                           
028600                                                                          
028700     IF IMS-EJ-OK                                                         
028800       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
028900       DISPLAY FELTEXT                                                    
029000       CALL FELLOG                                                        
029100     END-IF                                                               
029200     .                                                                    
029300     SKIP2                                                                
032500 IMS-STATUSKONTROLL SECTION.                                              
032600     SKIP2                                                                
032700     SET STATUS-IX TO 1                                                   
032800     SEARCH GODK-STATUS                                                   
032900       AT END                                                             
033000         MOVE 'FELAKTIG STATUSKOD FRÅN IMS' TO FELTEXT-STR                
033100         DISPLAY FELTEXT                                                  
033200         CALL FELLOG                                                      
033300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033400         CONTINUE                                                         
033500     END-SEARCH                                                           
033600     .                                                                    
