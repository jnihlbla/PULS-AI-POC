000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     TP8LOAD.                                                 
000300 AUTHOR.         PRIYA RC.                                                
000400 DATE-WRITTEN.   17/08/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CREATE LOAD FILES FOR NEW TABLES TP8GRET AND TP8TRET             
001000*                                                                         
001100     SKIP3                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP2                                                                
001400 INPUT-OUTPUT SECTION.                                                    
001500                                                                          
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800*          --- INPUT FILE 1                                               
001900     SELECT FRETFILE             ASSIGN TO WFRETFD1.                      
002000     SKIP2                                                                
002200*             INPUT FILE 2                                                
002300     SELECT DISTFIL1             ASSIGN TO WDISTFD1.                      
002400     SKIP2                                                                
002500*             OUTPUT FILE1- LOAD FILE TO TP8GRET                          
002600     SELECT GRETFILE             ASSIGN TO WGRETFD3.                      
002700     SKIP2                                                                
002710     SKIP2                                                                
002720*          --- INPUT FILE 3                                               
002730     SELECT SRETFILE             ASSIGN TO WSRETFD1.                      
002740     SKIP2                                                                
002750*             INPUT FILE 4                                                
002760     SELECT DISTFIL2             ASSIGN TO WDISTFD2.                      
002770     SKIP2                                                                
002780*             OUTPUT FILE2- LOAD FILE TO TP8TRET                          
002790     SELECT TRETFILE             ASSIGN TO WTRETFD3.                      
002791     SKIP2                                                                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  FRETFILE                                                             
003400     RECORDING       V                                                    
003500     BLOCK CONTAINS  0.                                                   
003600 01  FRETFILE-REC                PIC X(4088).                             
003700     SKIP3                                                                
003800 FD  DISTFIL1                                                             
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100 01  DIST-REC1                   PIC X(15).                               
004110     SKIP3                                                                
004200 FD  GRETFILE                                                             
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500 01  GRET-REC                    PIC X(55).                               
004510     SKIP3                                                                
004520 FD  SRETFILE                                                             
004530     RECORDING       V                                                    
004540     BLOCK CONTAINS  0.                                                   
004550 01  FRETFILE-REC                PIC X(4088).                             
004560     SKIP3                                                                
004570 FD  DISTFIL2                                                             
004580     RECORDING       F                                                    
004590     BLOCK CONTAINS  0.                                                   
004591 01  DIST-REC2                   PIC X(15).                               
004592     SKIP3                                                                
004593 FD  TRETFILE                                                             
004594     RECORDING       F                                                    
004595     BLOCK CONTAINS  0.                                                   
004596 01  TRET-REC                    PIC X(56).                               
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
005000                                                                          
006000 77  IDPGM                       PIC X(8)    VALUE 'TP8LOAD'.             
006100 77  YES                         PIC X       VALUE 'J'.                   
006200 77  NOO                         PIC X       VALUE 'N'.                   
006400 77  FRET-EOF-SW                 PIC X       VALUE 'N'.                   
006500     88  END-OF-FRET                         VALUE 'Y'.                   
006600 77  DIST1-EOF-SW                 PIC X       VALUE 'N'.                  
006700     88  END-OF-DIST1                        VALUE 'Y'.                   
006710 77  SRET-EOF-SW                 PIC X       VALUE 'N'.                   
006720     88  END-OF-SRET                         VALUE 'Y'.                   
006730 77  DIST2-EOF-SW                 PIC X       VALUE 'N'.                  
006740     88  END-OF-DIST2                        VALUE 'Y'.                   
006800 01  DYNAMISKA-SUBPROGRAM.                                                
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200                                                                          
007301 01  WS-WORK.                                                             
007401     03  WS-DIST-TABLE.                                                   
007501         05 DIST-TABLE      OCCURS 200 TIMES INDEXED BY DIST-IX.          
007601            07 TAB-IDPARTNR       PIC X(9).                               
007701            07 TAB-DIST           PIC X(4).                               
007801     03  WS-IDPARTNR              PIC X(9).                               
007901     03  WS-IDDISTR               PIC X(4).                               
008001     03  WS-IDDISTR-NUM           PIC 9(4).                               
008101     03  WS-SUARTBTO-MIN          PIC 9(3).                               
008102     03  WS-PRARTNTO              PIC 9(3).                               
008103     03  WS-PRARTNTO-LDC          PIC 9(3).                               
009001     EJECT                                                                
034000 01  FELTEXT.                                                             
034100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
034200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
034300     EJECT                                                                
034400 01  FILLER                  PIC X(16)   VALUE 'IN-FRET    '.             
034500 01  IN-FRET.                                                             
034600     03  IN-IDPARTNR         PIC X(9).                                    
034700     03  FILLER              PIC X(2).                                    
034800     03  IN-KDANMORS         PIC X(2).                                    
034900     03  FILLER              PIC X(14).                                   
035001     03  IN-SUARTBTO-MIN     PIC X(3).                                    
035101     03  IN-SUARTBTO-MIN-DEC PIC X(3).                                    
035201     03  FILLER              PIC X(2).                                    
035300     03  IN-FLINVLDC         PIC X(1).                                    
035400     03  FILLER              PIC X(21).                                   
035500     03  IN-SUARTBTO-LDC-MIN PIC X(4).                                    
035600     03  FILLER              PIC X(2).                                    
035700     03  IN-IDUSER           PIC X(8).                                    
035800     03  FILLER              PIC X(2).                                    
035900     03  IN-DAREGDAT         PIC X(8).                                    
036000     03  FILLER              PIC X(2).                                    
036100     03  IN-DAUPPDAT         PIC X(8).                                    
036200     03  FILLER              PIC X(2).                                    
036300     03  IN-IDFTG            PIC X(2).                                    
036400     EJECT                                                                
036500                                                                          
036600 01  FILLER                  PIC X(16)   VALUE 'IN2-DIST   '.             
036700 01  IN2-DIST.                                                            
036800     03  FILLER              PIC X(1).                                    
036900     03  IN2-IDPARTNR        PIC X(9).                                    
037000     03  FILLER              PIC X(1).                                    
037100     03  IN2-IDDISTR         PIC X(4).                                    
037110     EJECT                                                                
037200 01  FILLER                  PIC X(16)   VALUE 'UT-GRET    '.             
037300 01  UT-GRET.                                                             
037801     03  UT-IDDISTR          PIC S9(5) COMP-3.                            
038001     03  UT-IDKUNDNR         PIC S9(7) COMP-3.                            
038101     03  UT-KDANMORS         PIC X(2).                                    
038201     03  UT-FLINVLDC         PIC X(1).                                    
038301     03  UT-SUARTBTO-MIN     PIC S9(7)V9(2) COMP-3.                       
038401     03  UT-SUARTBTO-LDC-MIN PIC S9(7)V9(2) COMP-3.                       
038501     03  UT-IDPARTNR         PIC X(9).                                    
038601     03  UT-IDUSER           PIC X(8).                                    
038701     03  UT-DAREGDAT         PIC X(8).                                    
038801     03  UT-DAUPPDAT         PIC X(8).                                    
038901     03  UT-IDFTG            PIC X(2).                                    
039000     EJECT                                                                
039010 01  IN-SRET.                                                             
039020     03  INS-IDPARTNR         PIC X(9).                                   
039030     03  FILLER               PIC X(2).                                   
039040     03  INS-KDANMORS         PIC X(2).                                   
039050     03  FILLER               PIC X(8).                                   
039052     03  INS-FLINVFEE         PIC X(1).                                   
039053     03  FILLER               PIC X(14).                                  
039060     03  INS-PRARTNTO         PIC X(3).                                   
039070     03  INS-PRARTNTO-DEC     PIC X(3).                                   
039091     03  FILLER               PIC X(8).                                   
039092     03  INS-PRARTNTO-LDC     PIC X(3).                                   
039093     03  INS-PRARTNTO-LDC-DEC PIC X(3).                                   
039094     03  FILLER               PIC X(2).                                   
039095     03  INS-FLINVLDC         PIC X(1).                                   
039096     03  FILLER               PIC X(9).                                   
039097     03  INS-IDUSER           PIC X(8).                                   
039098     03  FILLER               PIC X(2).                                   
039099     03  INS-DAREGDAT         PIC X(8).                                   
039100     03  FILLER               PIC X(2).                                   
039101     03  INS-DAUPPDAT         PIC X(8).                                   
039102     03  FILLER               PIC X(2).                                   
039103     03  INS-IDFTG            PIC X(2).                                   
039104     EJECT                                                                
039105                                                                          
039106 01  FILLER                  PIC X(16)   VALUE 'IN2-DIST   '.             
039107 01  IN2-DIST2.                                                           
039108     03  FILLER              PIC X(1).                                    
039109     03  IN2-IDPARTNR2       PIC X(9).                                    
039110     03  FILLER              PIC X(1).                                    
039111     03  IN2-IDDISTR2        PIC X(4).                                    
039112     EJECT                                                                
039113 01  FILLER                  PIC X(16)   VALUE 'UT2-TRET   '.             
039114 01  UT2-TRET.                                                            
039115     03  UT2-IDDISTR          PIC S9(5) COMP-3.                           
039116     03  UT2-IDKUNDNR         PIC S9(7) COMP-3.                           
039117     03  UT2-KDANMORS         PIC X(2).                                   
039118     03  UT2-PRARTNTO         PIC S9(7)V9(2) COMP-3.                      
039119     03  UT2-PRARTNTO-LDC     PIC S9(7)V9(2) COMP-3.                      
039120     03  UT2-FLINVFEE         PIC X(1).                                   
039121     03  UT2-FLINVLDC         PIC X(1).                                   
039122     03  UT2-IDPARTNR         PIC X(9).                                   
039123     03  UT2-IDUSER           PIC X(8).                                   
039124     03  UT2-DAREGDAT         PIC X(8).                                   
039125     03  UT2-DAUPPDAT         PIC X(8).                                   
039126     03  UT2-IDFTG            PIC X(2).                                   
039127                                                                          
039130*01  -COPY W0005   -PRE  POSTSUM-                                         
039200     EJECT                                                                
039300 PROCEDURE DIVISION.                                                      
039400 MAIN SECTION.                                                            
039500                                                                          
039600     PERFORM A-INIT                                                       
039610     INITIALIZE WS-WORK                                                   
039700*    PERFORM B0-GRET-LOAD-PROCESSING                                      
039701                                                                          
039710     INITIALIZE WS-WORK                                                   
039800     PERFORM C0-TRET-LOAD-PROCESSING                                      
042901                                                                          
043001     PERFORM Z-FINIT                                                      
043101                                                                          
043201     MOVE ZERO                   TO RETURN-CODE                           
043301     GOBACK                                                               
043401     .                                                                    
043501     EJECT                                                                
043601                                                                          
043701 A-INIT SECTION.                                                          
043801     OPEN INPUT  FRETFILE                                                 
043901                 DISTFIL1                                                 
043902                 SRETFILE                                                 
043903                 DISTFIL2                                                 
044001     OPEN OUTPUT GRETFILE                                                 
044002                 TRETFILE                                                 
044101                                                                          
044201*    MOVE IDPGM                 TO POSTSUM-PROGNAMN                       
044301     .                                                                    
044401     EJECT                                                                
044501                                                                          
044502 B0-GRET-LOAD-PROCESSING SECTION.                                         
044503     DISPLAY '***************GRET PROCESSING'                             
044504     PERFORM S01-READ-FRETFILE                                            
044505     PERFORM UNTIL END-OF-FRET OR END-OF-DIST1                            
044506         DISPLAY 'IN-IDPARTNR:' IN-IDPARTNR '/WS-IDPARTNR:'               
044507         WS-IDPARTNR                                                      
044508         IF IN-IDPARTNR = WS-IDPARTNR                                     
044509            PERFORM BBA-POPULATE-GRET                                     
044510         ELSE                                                             
044511            PERFORM B-PROCESSING                                          
044512         END-IF                                                           
044513         PERFORM S01-READ-FRETFILE                                        
044514     END-PERFORM                                                          
044515     .                                                                    
044516     EJECT                                                                
044520                                                                          
044601 B-PROCESSING SECTION.                                                    
044602     DISPLAY 'B-PROCESSING IN-IDPARTNR:' IN-IDPARTNR                      
044603        'IN2-IDPARTNR:' IN2-IDPARTNR                                      
044701                                                                          
044702     IF IN-IDPARTNR < IN2-IDPARTNR                                        
044703        DISPLAY 'PARMA NUMBER ' IN-IDPARTNR 'NO DISTR'                    
044705         CONTINUE                                                         
044706     ELSE                                                                 
044801        IF IN2-IDPARTNR = IN-IDPARTNR                                     
044802           DISPLAY 'BOTH PARMA NUMBER SAME 1'                             
044901           INITIALIZE WS-DIST-TABLE                                       
045001           SET DIST-IX   TO +1                                            
045101           MOVE IN-IDPARTNR    TO WS-IDPARTNR                             
045201           MOVE IN2-IDDISTR    TO WS-IDDISTR                              
045301           PERFORM UNTIL IN2-IDPARTNR NOT = WS-IDPARTNR OR                
045302                         END-OF-DIST1                                     
045401              MOVE  IN2-IDPARTNR    TO TAB-IDPARTNR(DIST-IX)              
045501              MOVE  IN2-IDDISTR     TO TAB-DIST(DIST-IX)                  
045601              SET DIST-IX UP BY +1                                        
045701              PERFORM S02-READ-DISTFIL1                                   
045801           END-PERFORM                                                    
045802           PERFORM BBA-POPULATE-GRET                                      
045901        ELSE                                                              
045902           DISPLAY 'READ DISTFIL1 UNTIL PARTNR SAME 2'                    
046003           PERFORM UNTIL IN2-IDPARTNR >= IN-IDPARTNR OR                   
046004               END-OF-DIST1                                               
046103              PERFORM S02-READ-DISTFIL1                                   
046203           END-PERFORM                                                    
046204           IF IN2-IDPARTNR > IN-IDPARTNR                                  
046205               DISPLAY IN2-IDPARTNR '>' IN-IDPARTNR 'NO DIST'             
046206               CONTINUE                                                   
046207           ELSE                                                           
046208              IF IN2-IDPARTNR = IN-IDPARTNR                               
046209                 DISPLAY 'BOTH PARMA NUMBER SAME 3'                       
046210                 INITIALIZE WS-DIST-TABLE                                 
046211                 SET DIST-IX   TO +1                                      
046220                 MOVE IN-IDPARTNR    TO WS-IDPARTNR                       
046230                 MOVE IN2-IDDISTR    TO WS-IDDISTR                        
046240                 PERFORM UNTIL IN2-IDPARTNR NOT = WS-IDPARTNR OR          
046250                               END-OF-DIST1                               
046260                    MOVE  IN2-IDPARTNR    TO TAB-IDPARTNR(DIST-IX)        
046270                    MOVE  IN2-IDDISTR     TO TAB-DIST(DIST-IX)            
046280                    SET DIST-IX UP BY +1                                  
046290                    PERFORM S02-READ-DISTFIL1                             
046300                 END-PERFORM                                              
046301                 PERFORM BBA-POPULATE-GRET                                
046302              END-IF                                                      
046303           END-IF                                                         
046307*          INITIALIZE                                                     
046401*          SET DIST-IX   TO +1                                            
046501*          MOVE IN-IDPARTNR    TO WS-IDPARTNR                             
046601*          MOVE IN2-IDDISTR    TO WS-IDDISTR                              
046702*          PERFORM UNTIL IN2-IDPARTNR NOT = WS-IDPARTNR OR                
046703*                        END-OF-DIST                                      
046801*             MOVE  IN2-IDPARTNR    TO TAB-IDPARTNR(DIST-IX)              
046901*             MOVE  IN2-IDDISTR     TO TAB-DIST(DIST-IX)                  
047001*             SET DIST-IX UP BY +1                                        
047101*             PERFORM S02-READ-DISTFIL1                                   
047201*          END-PERFORM                                                    
047301        END-IF                                                            
047502     END-IF                                                               
047601     .                                                                    
047701     EJECT                                                                
047801                                                                          
047901 BBA-POPULATE-GRET SECTION.                                               
047902*    DISPLAY 'BBA-POPULATE-GRET'                                          
048001     SET DIST-IX TO +1                                                    
048101     PERFORM UNTIL TAB-DIST(DIST-IX) = SPACES                             
048201         PERFORM S03-MOVE-UTGRET                                          
048301         SET DIST-IX UP BY +1                                             
048401     END-PERFORM                                                          
048501     .                                                                    
048601     EJECT                                                                
048602                                                                          
048603 C0-TRET-LOAD-PROCESSING SECTION.                                         
048604     DISPLAY '********************TRET PROCESSING'                        
048605     PERFORM S11-READ-SRETFILE                                            
048606     PERFORM UNTIL END-OF-SRET OR END-OF-DIST2                            
048607         DISPLAY 'IN-IDPARTNR:' INS-IDPARTNR '/WS-IDPARTNR:'              
048608         WS-IDPARTNR                                                      
048609         IF INS-IDPARTNR = WS-IDPARTNR                                    
048610            PERFORM CCA-POPULATE-TRET                                     
048611         ELSE                                                             
048620            PERFORM C-PROCESSING                                          
048630         END-IF                                                           
048640         PERFORM S11-READ-SRETFILE                                        
048650     END-PERFORM                                                          
048660     .                                                                    
048670     EJECT                                                                
048680 C-PROCESSING SECTION.                                                    
048690     DISPLAY 'C-PROCESSING INS-IDPARTNR:' INS-IDPARTNR                    
048700        'IN2-IDPARTNR2:' IN2-IDPARTNR2                                    
048800                                                                          
048801     IF INS-IDPARTNR < IN2-IDPARTNR2                                      
048802        DISPLAY 'PARMA NUM:' INS-IDPARTNR 'NO DISTR' INS-KDANMORS         
048803         CONTINUE                                                         
048804     ELSE                                                                 
048805        IF IN2-IDPARTNR2 = INS-IDPARTNR                                   
048806           DISPLAY 'BOTH PARMA NUMBER SAME 1'                             
048807           INITIALIZE WS-DIST-TABLE                                       
048808           SET DIST-IX   TO +1                                            
048809           MOVE INS-IDPARTNR    TO WS-IDPARTNR                            
048810           MOVE IN2-IDDISTR2    TO WS-IDDISTR                             
048811           PERFORM UNTIL IN2-IDPARTNR2 NOT = WS-IDPARTNR OR               
048812                         END-OF-DIST2                                     
048813              MOVE  IN2-IDPARTNR2    TO TAB-IDPARTNR(DIST-IX)             
048814              MOVE  IN2-IDDISTR2     TO TAB-DIST(DIST-IX)                 
048815              SET DIST-IX UP BY +1                                        
048816              PERFORM S12-READ-DISTFIL2                                   
048817           END-PERFORM                                                    
048818           PERFORM CCA-POPULATE-TRET                                      
048819        ELSE                                                              
048820           DISPLAY 'READ DISTFIL2 UNTIL PARTNR SAME 2'                    
048821           PERFORM UNTIL IN2-IDPARTNR2 >= INS-IDPARTNR OR                 
048822               END-OF-DIST2                                               
048823              PERFORM S12-READ-DISTFIL2                                   
048824           END-PERFORM                                                    
048825           IF IN2-IDPARTNR2 > INS-IDPARTNR                                
048826               DISPLAY IN2-IDPARTNR2 '>' INS-IDPARTNR 'NO DIST'           
048827                '/' INS-KDANMORS                                          
048828               CONTINUE                                                   
048829           ELSE                                                           
048830              IF IN2-IDPARTNR2 = INS-IDPARTNR                             
048831                 DISPLAY 'BOTH PARMA NUMBER SAME 3'                       
048832                 INITIALIZE WS-DIST-TABLE                                 
048833                 SET DIST-IX   TO +1                                      
048834                 MOVE INS-IDPARTNR    TO WS-IDPARTNR                      
048835                 MOVE IN2-IDDISTR2    TO WS-IDDISTR                       
048836                 PERFORM UNTIL IN2-IDPARTNR2 NOT = WS-IDPARTNR OR         
048837                               END-OF-DIST2                               
048838                    MOVE  IN2-IDPARTNR2   TO TAB-IDPARTNR(DIST-IX)        
048839                    MOVE  IN2-IDDISTR2    TO TAB-DIST(DIST-IX)            
048840                    SET DIST-IX UP BY +1                                  
048841                    PERFORM S12-READ-DISTFIL2                             
048842                 END-PERFORM                                              
048843                 PERFORM CCA-POPULATE-TRET                                
048844              END-IF                                                      
048845           END-IF                                                         
048856        END-IF                                                            
048857     END-IF                                                               
048858     .                                                                    
048859     EJECT                                                                
048860                                                                          
048861 CCA-POPULATE-TRET SECTION.                                               
048862*    DISPLAY 'CCA-POPULATE-TRET'                                          
048863     SET DIST-IX TO +1                                                    
048864     PERFORM UNTIL TAB-DIST(DIST-IX) = SPACES                             
048865         PERFORM S13-MOVE-UT2TRET                                         
048866         SET DIST-IX UP BY +1                                             
048867     END-PERFORM                                                          
048868     .                                                                    
048869     EJECT                                                                
048870                                                                          
048880 Z-FINIT SECTION.                                                         
049102     CLOSE FRETFILE                                                       
049103           DISTFIL1                                                       
049104           SRETFILE                                                       
049105           DISTFIL2                                                       
049106           GRETFILE                                                       
049201           TRETFILE                                                       
049301*    MOVE 'S'                   TO POSTSUM-OPKOD                          
049401*    CALL POSTSUM             USING POSTSUM-PARM                          
049501     .                                                                    
049601     EJECT                                                                
049701 S01-READ-FRETFILE SECTION.                                               
049801     SKIP2                                                                
049901     READ FRETFILE INTO IN-FRET                                           
050001     AT END                                                               
050101        SET END-OF-FRET TO TRUE                                           
050201                                                                          
050301     NOT AT END                                                           
050401        DISPLAY IN-FRET                                                   
050501*       MOVE SLAG-IDDC  TO WS-IDDC                                        
050601*       MOVE 'W01184' TO POSTSUM-FDNAMN                                   
050701*       MOVE 'W51218D1' TO POSTSUM-DDNAMN2                                
050801*       CALL POSTSUM USING POSTSUM-PARM                                   
050901     END-READ                                                             
051001     .                                                                    
051101     EJECT                                                                
051201 S02-READ-DISTFIL1 SECTION.                                               
051301     SKIP2                                                                
051401     READ DISTFIL1 INTO IN2-DIST                                          
051501     AT END                                                               
051601        SET END-OF-DIST1 TO TRUE                                          
051701                                                                          
051801     NOT AT END                                                           
051802        CONTINUE                                                          
051901*       DISPLAY IN2-DIST                                                  
052001*       MOVE SLAG-IDDC  TO WS-IDDC                                        
052101*       MOVE 'W01184' TO POSTSUM-FDNAMN                                   
052201*       MOVE 'W51218D1' TO POSTSUM-DDNAMN2                                
052301*       CALL POSTSUM USING POSTSUM-PARM                                   
052401     END-READ                                                             
052501     .                                                                    
052601     EJECT                                                                
052701 S03-MOVE-UTGRET SECTION.                                                 
052801     MOVE TAB-DIST(DIST-IX)   TO WS-IDDISTR-NUM                           
052901     MOVE WS-IDDISTR-NUM      TO UT-IDDISTR                               
053001     MOVE 0                   TO UT-IDKUNDNR                              
053101     MOVE IN-KDANMORS         TO UT-KDANMORS                              
053201     MOVE IN-FLINVLDC         TO UT-FLINVLDC                              
053301     MOVE IN-SUARTBTO-MIN     TO WS-SUARTBTO-MIN                          
053401     MOVE WS-SUARTBTO-MIN     TO UT-SUARTBTO-MIN                          
053501*    MOVE IN-SUARTBTO-LDC-MIN TO UT-SUARTBTO-LDC-MIN                      
053601     MOVE 0                   TO UT-SUARTBTO-LDC-MIN                      
053701     MOVE TAB-IDPARTNR(DIST-IX)                                           
053801                              TO UT-IDPARTNR                              
053802*    DISPLAY 'DAREGDAT:' IN-DAREGDAT 'DAUPPDAT:' IN-DAUPPDAT              
053803*    'IN-IDFTG:' IN-IDFTG                                                 
053901     MOVE IN-IDUSER           TO UT-IDUSER                                
054001     MOVE IN-DAREGDAT         TO UT-DAREGDAT                              
054101     MOVE IN-DAUPPDAT         TO UT-DAUPPDAT                              
054201     MOVE IN-IDFTG            TO UT-IDFTG                                 
054202*    DISPLAY UT-IDPARTNR '/' WS-IDDISTR-NUM '/' UT-KDANMORS               
054203*    '/' UT-DAREGDAT '/' UT-DAUPPDAT '/' UT-IDFTG                         
054301     PERFORM S03A-WRITE-GRET                                              
054401     .                                                                    
055001     EJECT                                                                
060001 S03A-WRITE-GRET SECTION.                                                 
060002     DISPLAY 'S03A-WRITE-GRET' UT-IDPARTNR '/' WS-IDDISTR-NUM             
060003     '/' UT-KDANMORS                                                      
070001     WRITE GRET-REC    FROM UT-GRET                                       
110001     .                                                                    
120001     EJECT                                                                
120002 S11-READ-SRETFILE SECTION.                                               
120003     SKIP2                                                                
120004     READ SRETFILE INTO IN-SRET                                           
120005     AT END                                                               
120006        SET END-OF-SRET TO TRUE                                           
120007                                                                          
120008     NOT AT END                                                           
120009        DISPLAY IN-SRET                                                   
120050     END-READ                                                             
120060     .                                                                    
120070     EJECT                                                                
120080 S12-READ-DISTFIL2 SECTION.                                               
120090     SKIP2                                                                
120100     READ DISTFIL2 INTO IN2-DIST2                                         
120200     AT END                                                               
120300        SET END-OF-DIST2 TO TRUE                                          
120400                                                                          
120500     NOT AT END                                                           
120600        CONTINUE                                                          
121200     END-READ                                                             
121300     .                                                                    
121310 S13-MOVE-UT2TRET SECTION.                                                
121320     MOVE TAB-DIST(DIST-IX)   TO WS-IDDISTR-NUM                           
121330     MOVE WS-IDDISTR-NUM      TO UT2-IDDISTR                              
121340     MOVE 0                   TO UT2-IDKUNDNR                             
121350     MOVE INS-KDANMORS        TO UT2-KDANMORS                             
121360     MOVE INS-FLINVLDC        TO UT2-FLINVLDC                             
121361     MOVE INS-FLINVFEE        TO UT2-FLINVFEE                             
121370     MOVE INS-PRARTNTO        TO WS-PRARTNTO                              
121380     MOVE WS-PRARTNTO         TO UT2-PRARTNTO                             
121391     MOVE INS-PRARTNTO-LDC    TO WS-PRARTNTO-LDC                          
121392     MOVE WS-PRARTNTO-LDC     TO UT2-PRARTNTO-LDC                         
121393     MOVE TAB-IDPARTNR(DIST-IX)                                           
121394                              TO UT2-IDPARTNR                             
121395*    DISPLAY 'DAREGDAT:' IN-DAREGDAT 'DAUPPDAT:' IN-DAUPPDAT              
121396*    'IN-IDFTG:' IN-IDFTG                                                 
121397     MOVE INS-IDUSER           TO UT2-IDUSER                              
121398     MOVE INS-DAREGDAT         TO UT2-DAREGDAT                            
121399     MOVE INS-DAUPPDAT         TO UT2-DAUPPDAT                            
121400     MOVE INS-IDFTG            TO UT2-IDFTG                               
121401*    DISPLAY UT-IDPARTNR '/' WS-IDDISTR-NUM '/' UT-KDANMORS               
121402*    '/' UT-DAREGDAT '/' UT-DAUPPDAT '/' UT-IDFTG                         
121403     PERFORM S13A-WRITE-TRET                                              
121404     .                                                                    
121405     EJECT                                                                
121410 S13A-WRITE-TRET SECTION.                                                 
121500     DISPLAY 'S13A-WRITE-TRET' UT2-IDPARTNR '/' WS-IDDISTR-NUM            
121600     '/' UT2-KDANMORS                                                     
121700     WRITE TRET-REC    FROM UT2-TRET                                      
121800     .                                                                    
121900     EJECT                                                                
