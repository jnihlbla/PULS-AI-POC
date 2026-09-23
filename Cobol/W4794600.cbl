000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W4794600.                                                
000500 AUTHOR.         ARCHANA BHAT.                                            
000600 DATE-WRITTEN.   15/05/04.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*                                                                         
001000*    FUNCTION:                                                            
001100*        THIS PROGRAM MAKES A LIST OF VALID DISTRICTS FROM THE            
001200*        DB2 TP4TRAN TABLE                                                
001300*                                                                         
001400*                                                                         
001500*    ABENDCODES:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- VALID DISTRICTS FROM TP4TRAN                               
002800     SELECT W47946                     ASSIGN TO W47946D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W47946                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  POST -COPY W47945 -PRE  W47946-  -L.                                 
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W4794600'.            
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004500 77  WS-TRNSFR                   PIC X(8)    VALUE 'TRANSFER'.            
004600     EJECT                                                                
440000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
450000 01  FILLER REDEFINES TODAYS-DATE.                                        
460000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
470000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
480000     03  TODAYS-DATE-DAY         PIC 9(2).                                
490000     EJECT                                                                
490001 01  GENERAL-SUBPROGRAMS.                                                 
490002*                                                                         
490003     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
490004     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
490005     SKIP2                                                                
490006 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
490007       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
490008                                                                          
490009 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
490010 01  DB2-WS.                                                              
490011     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
490012         88  CURSOR-OK                      VALUE 000.                    
490013         88  LINES-FOUND                    VALUE 000.                    
490014         88  LINES-MISSING                  VALUE 100.                    
490015         88  RESOURCE-WRONG                 VALUE 904.                    
490016     03  GOOD-SQLCODECODES.                                               
490017         05  GOOD-SQLCODE OCCURS 5                                        
490018             INDEXED BY SQLCODE-IX PIC 9(3).                              
490019 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
490020     EJECT                                                                
490021*    --- PARAMETERS TO ABEND                                              
490022                                                                          
490023 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
490024 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
490025 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
490026     SKIP2                                                                
490027 01  ERROR-TEXT.                                                          
490028     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
490029     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
490030     EJECT                                                                
490031*    --- PARAMETRAR TILL POSTSUM                                          
490032*                                                                         
490033*01  -COPY W0005   -PRE  POSTSUM-                                         
490034     EJECT                                                                
490035 01  UT-AREA-START               PIC X(24)   VALUE                        
490036                                 'UT-AREA-START  '.                       
490037     SKIP2                                                                
490038                                                                          
490039*01  AREA -COPY W47945     -PRE UT-                                       
490040     EJECT                                                                
490041 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
490042                                                                          
490043*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
490044     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
490045     EJECT                                                                
730600 PROCEDURE DIVISION.                                                      
730700 MAIN SECTION.                                                            
730800     SKIP2                                                                
730801                                                                          
730802     PERFORM A-INIT                                                       
730803     PERFORM B-GET-TP4TRAN-DATA                                           
730804                                                                          
730805     PERFORM Z-FINIT                                                      
730806                                                                          
730807     MOVE ZERO TO RETURN-CODE                                             
730808     GOBACK                                                               
730809     .                                                                    
730810     EJECT                                                                
730811 A-INIT SECTION.                                                          
730812                                                                          
730813     OPEN OUTPUT W47946                                                   
730814     SKIP2                                                                
860000     ACCEPT TODAYS-DATE  FROM DATE                                        
860001     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
860002     .                                                                    
860003     EJECT                                                                
860004 B-GET-TP4TRAN-DATA SECTION.                                              
860005                                                                          
860006     PERFORM DB2-OPEN-TP4TRAN-CRS                                         
860007     IF LINES-FOUND                                                       
860008        PERFORM DB2-FETCH-TP4TRAN-CRS                                     
860009     END-IF                                                               
860010                                                                          
860011     PERFORM UNTIL SQLCODE > ZERO                                         
860012        MOVE TP4TRAN-IDDC-SEND TO UT-IDDC                                 
860013        MOVE TP4TRAN-IDDISTR   TO UT-IDDISTR                              
860014        MOVE WS-TRNSFR         TO UT-DISTR-TYP                            
901103        MOVE SPACES            TO UT-IDLANDX2                             
901104        PERFORM S11-WRITE-W47946                                          
901105                                                                          
901106        PERFORM DB2-FETCH-TP4TRAN-CRS                                     
901107     END-PERFORM                                                          
901108                                                                          
901109     PERFORM DB2-CLOSE-TP4TRAN-CRS                                        
901110     .                                                                    
901111     EJECT                                                                
901112 Z-FINIT SECTION.                                                         
901113     CLOSE W47946                                                         
901114     SKIP2                                                                
901115     MOVE 'S' TO POSTSUM-OPKOD                                            
901116     CALL POSTSUM USING POSTSUM-PARM                                      
901117     .                                                                    
901118     EJECT                                                                
901119 S11-WRITE-W47946 SECTION.                                                
901120                                                                          
901121     WRITE W47946-POST FROM UT-AREA                                       
901122                                                                          
901123     MOVE 'W47946' TO POSTSUM-FDNAMN                                      
901124     MOVE 'W47946D1' TO POSTSUM-DDNAMN2                                   
901125     CALL POSTSUM USING POSTSUM-PARM                                      
901126     .                                                                    
901127     EJECT                                                                
901128 S99-ABEND SECTION.                                                       
901129                                                                          
901130     SKIP2                                                                
901131     MOVE 'S' TO POSTSUM-OPKOD                                            
901132     CALL POSTSUM USING POSTSUM-PARM                                      
901133     CALL ABEND USING RKOD-ABEND                                          
901134     .                                                                    
901135 DB2-OPEN-TP4TRAN-CRS SECTION.                                            
901136                                                                          
901137     EXEC SQL DECLARE TP4TRAN-CRS CURSOR FOR                              
901138         SELECT  IDDC_SEND                                                
901139                ,IDDISTR                                                  
901140                                                                          
901141         FROM    TP4TRAN                                                  
901142                                                                          
901143         WHERE KDARBTYP = 'ESC' OR                                        
901144               KDARBTYP = 'QUAL'                                          
901145                                                                          
901146         ORDER BY IDDC_SEND                                               
901147                                                                          
901148     END-EXEC                                                             
901149                                                                          
901150     MOVE SQLCODE TO SQLCODE-WS                                           
901151     MOVE 000     TO GOOD-SQLCODECODES                                    
901152                                                                          
901153     EXEC SQL OPEN TP4TRAN-CRS                                            
901154     END-EXEC                                                             
901155                                                                          
901156     PERFORM DB2-STATUS-CHECK                                             
901157     .                                                                    
901158     EJECT                                                                
901159 DB2-FETCH-TP4TRAN-CRS SECTION.                                           
901160                                                                          
901161     MOVE 000100  TO GOOD-SQLCODECODES                                    
901162     EXEC SQL FETCH TP4TRAN-CRS INTO                                      
901163                :TP4TRAN-IDDC-SEND                                        
901164               ,:TP4TRAN-IDDISTR                                          
901165     END-EXEC                                                             
901166                                                                          
901167     MOVE SQLCODE TO SQLCODE-WS                                           
901168     PERFORM DB2-STATUS-CHECK                                             
901169     .                                                                    
901170     SKIP3                                                                
901171                                                                          
901172 DB2-CLOSE-TP4TRAN-CRS SECTION.                                           
901173                                                                          
901174     EXEC SQL CLOSE TP4TRAN-CRS                                           
901175     END-EXEC                                                             
901176     .                                                                    
901177     EJECT                                                                
901179 DB2-STATUS-CHECK  SECTION.                                               
901180                                                                          
901181     SET SQLCODE-IX TO 1                                                  
901182     SEARCH GOOD-SQLCODE                                                  
901183       AT END                                                             
901184          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
901185          DELIMITED BY SIZE INTO ERROR-TEXT                               
901186          CALL ABEND USING RKOD-ABEND-DB2                                 
901187       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
901188          CONTINUE                                                        
901189     END-SEARCH                                                           
901190     .                                                                    
901200     EJECT                                                                
