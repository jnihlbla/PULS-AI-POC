000100*COMPOPT DB2BIND=YES                                                      
001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WZ141000.                                                
001400 AUTHOR.         ANDRE KJELL.                                             
001500 DATE-WRITTEN.   02/09/16.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        GENERAL PROGRAM TO A REPORT FILE                                 
002101*        TO DISTRIBUTION & PRINT                                          
002110*                                                                         
002200*        A CONTROL FILE CONTAINING RECEIVING ADDRESS                      
002300*        AND OPTIONALLY RETURN ADDRESS, IS ALSO READ.                     
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- INPUT DATA                                                 
003403     SELECT INDATA                     ASSIGN TO WZ1410D1.                
003404     SKIP2                                                                
003405*          --- CONTROL FILE                                               
003410     SELECT CTLDATA                    ASSIGN TO WZ1410D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  INDATA                                                               
004003     RECORDING       V                                                    
004004     RECORD IS VARYING FROM 1 TO 3000 CHARACTERS                          
004005            DEPENDING ON IN-KVDLEN                                        
004006     BLOCK CONTAINS  0.                                                   
004007                                                                          
004008 01  FILLER                     PIC X(3000).                              
004009     SKIP3                                                                
004010 FD  CTLDATA                                                              
004011     RECORDING       F                                                    
004012     BLOCK CONTAINS  0.                                                   
004013                                                                          
004020 01  FILLER                     PIC X(80).                                
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'WZ141000'.            
004500 77  YES                         PIC X       VALUE 'J'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004700     SKIP2                                                                
004800 01  ERRTEXT.                                                             
004900     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005000     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005100 77  KDRC-DISPLAY                PIC Z(3)9.                               
005201                                                                          
005202 77  INDATA-EOF-SW               PIC X       VALUE 'N'.                   
005203     88  END-OF-INDATA                       VALUE 'Y'.                   
005204                                                                          
005205 77  CTLDATA-EOF-SW              PIC X       VALUE 'N'.                   
005210     88  END-OF-CTLDATA                      VALUE 'Y'.                   
005220                                                                          
005230 77  DAP-ADDRESS                 PIC X(50)   VALUE                        
005240                                 'CARPARTS.DAP.DISTRDOC'.                 
005250                                                                          
005500     EJECT                                                                
006200 01  GENERAL-SUBPROGRAMS.                                                 
006300*                                                                         
006500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006610     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
006620     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
006701     SKIP3                                                                
006702*    --- PARAMETERS TO ABEND                                              
006703                                                                          
006704 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006705 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006706 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006720     EJECT                                                                
006730*    --- PARAMETERS TO POSTSUM                                            
006740*                                                                         
006750*01  -COPY W0005   -PRE  POSTSUM-                                         
007001     EJECT                                                                
007002 01  IN-AREA-START               PIC X(16)   VALUE  'IN-AREA'.            
007004                                                                          
007005 01  IN-KVDLEN                   PIC 9(9)   BINARY.                       
007006                                                                          
007007 01  IN-AREA                     PIC X(3000).                             
007010     EJECT                                                                
007011 01  CTL-AREA-START              PIC X(16)   VALUE 'CTL-AREA'.            
007012                                                                          
007013     SKIP2                                                                
007020 01  CTL-AREA                    PIC X(80).                               
007050     EJECT                                                                
007060 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
007070     SKIP3                                                                
007071 01  HDR-KVDLEN                  PIC 9(9)   BINARY.                       
007072                                                                          
007080 01  HDR-AREA.                                                            
007081     03 -COPY WZ01REQU -PRE HDR-                                          
007082     03 -COPY WZ04HDR                                                     
007090     EJECT                                                                
007100 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
007110     SKIP3                                                                
007120 01  -COPY WZ01SEND                                                       
007200     EJECT                                                                
008100     SKIP3                                                                
008200 01  SEND-AREA                   PIC X(3000).                             
008500     SKIP3                                                                
009700                                                                          
010901 PROCEDURE DIVISION.                                                      
010902 MAIN SECTION.                                                            
011000                                                                          
011200     SKIP2                                                                
011300     PERFORM A-INIT                                                       
011400     PERFORM S04-SEND-OPEN                                                
011401     PERFORM S04-SEND-HEADER-RECORD                                       
011402                                                                          
011406     PERFORM S01-READ-INDATA                                              
011500     PERFORM UNTIL END-OF-INDATA                                          
011610       PERFORM S04-SEND-MESSAGE-FROM-INAREA                               
012201       PERFORM S01-READ-INDATA                                            
012300     END-PERFORM                                                          
012301                                                                          
012310     PERFORM S04-SEND-CLOSE                                               
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013401                                                                          
013402     OPEN INPUT INDATA CTLDATA                                            
013900                                                                          
014010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014020                                                                          
014021*    -- FETCH DATA FOR D&P HEADER                                         
014030     PERFORM S02-READ-CTLDATA                                             
014040     IF END-OF-CTLDATA                                                    
014041       STRING 'WZ1410  OUTPUT TYPE MISSING'                               
014042       DELIMITED BY SIZE INTO ERRTEXT                                     
014044     ELSE                                                                 
014045       MOVE CTL-AREA  TO HDR-IDOUTTYPE                                    
014046       PERFORM S02-READ-CTLDATA                                           
014050     END-IF                                                               
014240                                                                          
014251     IF END-OF-CTLDATA                                                    
014252       MOVE SPACE     TO HDR-IDOUTREC                                     
014255     ELSE                                                                 
014256       MOVE CTL-AREA  TO HDR-IDOUTREC                                     
014257       PERFORM S02-READ-CTLDATA                                           
014258     END-IF                                                               
014259                                                                          
014261     IF END-OF-CTLDATA                                                    
014262       MOVE FUNCTION CURRENT-DATE(3:12) TO HDR-IDLIST                     
014264     ELSE                                                                 
014265       MOVE CTL-AREA  TO HDR-IDLIST                                       
014266     END-IF                                                               
014267                                                                          
014268     IF ERRTEXT-STR NOT = SPACE                                           
014269       DISPLAY  ERRTEXT                                                   
014270       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
014271     END-IF                                                               
014272                                                                          
014273     MOVE '001'        TO HDR-REQU-IDMSGVER                               
014274     MOVE SPACE        TO HDR-REQU-KDPGMACT                               
014275     MOVE 'WZ1410'     TO HDR-REQU-IDUSER                                 
014280     MOVE DAP-ADDRESS  TO SEND-ADDISPABS                                  
014300     .                                                                    
014400     EJECT                                                                
014500 Z-FINIT SECTION.                                                         
014701                                                                          
014702     CLOSE INDATA CTLDATA                                                 
014703                                                                          
014901     SKIP2                                                                
014902     MOVE 'S' TO POSTSUM-OPKOD                                            
014910     CALL POSTSUM USING POSTSUM-PARM                                      
015100     .                                                                    
015201     EJECT                                                                
015202 S01-READ-INDATA  SECTION.                                                
015203     SKIP2                                                                
015204     READ INDATA INTO IN-AREA                                             
015205     AT END                                                               
015207        SET END-OF-INDATA TO TRUE                                         
015208                                                                          
015209     NOT AT END                                                           
015210        MOVE 'INDATA'   TO POSTSUM-FDNAMN                                 
015211        MOVE 'WZ1410D1' TO POSTSUM-DDNAMN2                                
015212        MOVE SPACE      TO POSTSUM-TRANSTYP                               
015213        CALL POSTSUM USING POSTSUM-PARM                                   
015214     END-READ                                                             
015215     .                                                                    
015216     EJECT                                                                
015217 S02-READ-CTLDATA  SECTION.                                               
015218     SKIP2                                                                
015219     READ CTLDATA INTO CTL-AREA                                           
015220     AT END                                                               
015222        SET END-OF-CTLDATA TO TRUE                                        
015223                                                                          
015224     NOT AT END                                                           
015225        MOVE 'CTLDATA'  TO POSTSUM-FDNAMN                                 
015226        MOVE 'WZ1410D2' TO POSTSUM-DDNAMN2                                
015227        MOVE SPACE      TO POSTSUM-TRANSTYP                               
015228        CALL POSTSUM USING POSTSUM-PARM                                   
015229     END-READ                                                             
015230     .                                                                    
015240     EJECT                                                                
015250 S04-SEND-OPEN SECTION.                                                   
015260                                                                          
015270     MOVE 'OPEN'                     TO SEND-KDFUNC                       
015290     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
015300                                                                          
015400     IF SEND-KDRC > 0                                                     
015500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
015600       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
015700       DELIMITED BY SIZE INTO ERRTEXT                                     
015800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015900     END-IF                                                               
016000     .                                                                    
016100     SKIP3                                                                
016200 S04-SEND-MESSAGE-FROM-INAREA SECTION.                                    
016300                                                                          
016400     MOVE 'PUT'                      TO SEND-KDFUNC                       
016600     CALL WZ01SEND USING SEND-CONTROL-AREA IN-KVDLEN IN-AREA              
016700                                                                          
016800     IF SEND-KDRC > 0                                                     
016900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
017000       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
017100       DELIMITED BY SIZE INTO ERRTEXT                                     
017200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
017300     END-IF                                                               
017400     .                                                                    
017410     SKIP3                                                                
017420 S04-SEND-HEADER-RECORD       SECTION.                                    
017430                                                                          
017440     MOVE 'PUT'                      TO SEND-KDFUNC                       
017441     MOVE LENGTH OF HDR-AREA         TO HDR-KVDLEN                        
017450     CALL WZ01SEND USING SEND-CONTROL-AREA HDR-KVDLEN HDR-AREA            
017460                                                                          
017470     IF SEND-KDRC > 0                                                     
017480       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
017490       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
017491       DELIMITED BY SIZE INTO ERRTEXT                                     
017492       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
017493     END-IF                                                               
017494     .                                                                    
017500     SKIP3                                                                
017600 S04-SEND-CLOSE SECTION.                                                  
017700                                                                          
017800     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
017900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
018000                                                                          
018100     IF SEND-KDRC > 0                                                     
018200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
018300       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
018400       DELIMITED BY SIZE INTO ERRTEXT                                     
018500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
018600     END-IF                                                               
018700     .                                                                    
