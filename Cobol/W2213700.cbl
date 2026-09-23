000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2213700.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   19/07/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        MATRIALFÖRSÖRJNING LOKAL ANSKAFFNING (KINA OCH USA)              
000900*                                                                         
001000*        THE PROGRAM READS     WDG3 (2203/2204)                           
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002700*          --- 2204-RECORDS                                               
002800     SELECT W2213701                   ASSIGN TO W22137D1.                
002900                                                                          
002910*          --- 2203-RECORDS                                               
002920     SELECT W2213702                   ASSIGN TO W22137D2.                
002930                                                                          
003000*          --- SORTERINGSFIL                                              
003100     SELECT SRT-FILE                   ASSIGN UT-S-W22130DS.              
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
004300 FD  W2213701                                                             
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  RECORD -COPY W2212204 -PRE  OUT-2204-  -L.                           
004800                                                                          
004810 FD  W2213702                                                             
004820     RECORDING       F                                                    
004830     BLOCK CONTAINS  0.                                                   
004840                                                                          
004850*01  RECORD -COPY W2242203 -PRE  OUT-2203-  -L.                           
004860     EJECT                                                                
004900 SD  SRT-FILE.                                                            
005000                                                                          
005100*01  POST  -COPY W2212204    -PRE SRT-                                    
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500 77  IDPGM                       PIC X(8)    VALUE 'W2213700'.            
005510 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
005520 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
005600 77  YES                         PIC X       VALUE 'J'.                   
005700 77  NOO                         PIC X       VALUE 'N'.                   
005800 01  W-IDARTNR                   PIC S9(9)   COMP-3 VALUE +0.             
005900 01  W-KDLPORS                   PIC S9(3)   COMP-3 VALUE +0.             
006100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES TODAYS-DATE.                                        
006300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006500     03  TODAYS-DATE-DAY         PIC 9(2).                                
006600     EJECT                                                                
006700 01  RKOD                    PIC S9(4)               COMP SYNC.           
006900 01  SWITCHAR.                                                            
007000     05  SW-SRT-FILE-EOF     PIC X       VALUE 'N'.                       
007110     EJECT                                                                
007200 01  GENERAL-SUBPROGRAMS.                                                 
007300*                                                                         
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007800     SKIP2                                                                
007900*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008000                                                                          
008100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008400     SKIP2                                                                
008500 01  ERROR-TEXT.                                                          
008600     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
008700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008800     EJECT                                                                
008900*    --- PARAMETRAR TILL POSTSUM                                          
009000*                                                                         
009100*01  -COPY W0005   -PRE  POSTSUM-                                         
009200     EJECT                                                                
009610 01  OUT-2203-AREA-START         PIC X(24)   VALUE                        
009620                                 'OUT-2203-AREA-START  '.                 
009630*01  AREA -COPY W2242203   -PRE OUT-2203-                                 
009640     EJECT                                                                
009700 01  OUT-2204-AREA-START         PIC X(24)   VALUE                        
009800                                 'OUT-2204-AREA-START  '.                 
010300*01  AREA -COPY W2212204   -PRE OUT-2204-                                 
010400     EJECT                                                                
010500*    --- AREAS FOR IMS-SECTIONS                                           
010600*                                                                         
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900 01  KEYS-FOR-DLI.                                                        
011000     03  W-WDGXKEY-2203-X.                                                
011100         05  W-IDHTYP            PIC X(04)   VALUE '2203'.                
011200         05  W-IDDC              PIC X(02)   VALUE '11'.                  
011300         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
011400                                                                          
011500 01  W-IDHTYP-2204               PIC X(04)   VALUE '2204'.                
011600                                                                          
011700*    --- STATUS-KOD FRÅN IMS                                              
011800 01  STATUS-WS                   PIC XX.                                  
011900     88  SEGMENT-FOUND                       VALUE '  '.                  
012000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012200     SKIP2                                                                
012300 01  GOOD-STATUSCODES.                                                    
012400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012500     SKIP3                                                                
012600 01  SSA1                        PIC X(64).                               
012700 01  SSA2                        PIC X(64).                               
012800     EJECT                                                                
012900*    --- IMS FUNCTION CODES                                               
013000*01  -COPY W0003                                                          
013100     EJECT                                                                
013200*    ---  DLI INPUT-OUTPUT AREA                                           
013300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2203'.                    
013400 01  DLI-IO-WDGX2203.                                                     
013500*    03  -COPY WDG301   -PRE 2203-.                                       
013600     EJECT                                                                
013700                                                                          
013800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2204'.                    
013900 01  DLI-IO-WDGX2204.                                                     
014000*    03  -COPY WDGX2204.                                                  
014100         05  FILLER PIC X(3).                                             
014200     EJECT                                                                
014300 LINKAGE SECTION.                                                         
014400                                                                          
014500*01  -COPY W0008  -PRE XXJB-                                              
014600     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014800 PROCEDURE DIVISION  USING XXJB-PCB.                                      
014900 MAIN SECTION.                                                            
015000     ENTRY 'DLITCBL' USING XXJB-PCB.                                      
015100                                                                          
015200     PERFORM A-INIT                                                       
015300                                                                          
015400     SORT SRT-FILE                                                        
015500          ASCENDING SRT-IDARTNR                                           
015600                    SRT-KDLPORS                                           
015700          INPUT PROCEDURE  B-LAES-WDG3                                    
015800          OUTPUT PROCEDURE C-SKRIV-UTFIL                                  
015900                                                                          
016000     IF SORT-RETURN > ZERO                                                
016100       DISPLAY '*** W22137, FEL I SORTERING'                              
016200       MOVE +20 TO RKOD                                                   
016300       CALL ABEND USING RKOD                                              
016400     ELSE                                                                 
016500       PERFORM Z-FINISH                                                   
016600       MOVE ZERO TO RETURN-CODE                                           
016700       GOBACK                                                             
016800     END-IF                                                               
016900     .                                                                    
017000     EJECT                                                                
017100******************************************************************        
017200*                                                                *        
017300*    INITIERING                                                  *        
017400*    ÖPPNA ALLA FILER                                            *        
017500*                                                                *        
017600******************************************************************        
017700                                                                          
017800 A-INIT       SECTION.                                                    
017900                                                                          
018000     OPEN OUTPUT W2213701                                                 
018100                 W2213702                                                 
018200                                                                          
018300     MOVE ZERO TO W-IDARTNR                                               
018400                  W-KDLPORS                                               
018500                                                                          
018600     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
018700     .                                                                    
018800     EJECT                                                                
018900 B-LAES-WDG3 SECTION.                                                     
019000**************************************************************            
019100*    HÄMTAR POSTER FRÅN WDG3                                 *            
019200**************************************************************            
019300                                                                          
019400     PERFORM IMS-GU-XXBJ01                                                
019500     IF SEGMENT-FOUND                                                     
019800       PERFORM IMS-GNP-XXBJ11                                             
019900       PERFORM UNTIL SEGMENT-MISSING                                      
020000                                                                          
020200         MOVE W-IDHTYP-2204 TO SRT-IDHTYP                                 
020300         MOVE 2204-IDARTNR  TO SRT-IDARTNR                                
020400         MOVE 2204-KDLPORS  TO SRT-KDLPORS                                
020500         RELEASE SRT-POST                                                 
020700                                                                          
020800         PERFORM IMS-GNP-XXBJ11                                           
020900       END-PERFORM                                                        
021000     END-IF                                                               
021100     .                                                                    
021200     EJECT                                                                
021300******************************************************************        
021400*                                                                *        
021500*    TAR BORT DUBLETTER OCH SKRIVER UTFIL                        *        
021600*                                                                *        
021700******************************************************************        
021800                                                                          
021900 C-SKRIV-UTFIL SECTION.                                                   
022000                                                                          
022100     PERFORM CA-LAES-SRT-FILE                                             
022102     IF SW-SRT-FILE-EOF = NOO                                             
022103       MOVE 2203-IDHTYP TO OUT-2203-IDHTYP                                
022104       MOVE W-IDDC      TO OUT-2203-IDDC                                  
022110       PERFORM S12-WRITE-W2213702                                         
022120     END-IF                                                               
022200                                                                          
022300     PERFORM UNTIL                                                        
022400      NOT ( SW-SRT-FILE-EOF = NOO )                                       
022500       IF  W-IDARTNR NOT = OUT-2204-IDARTNR                               
022600       OR  W-KDLPORS NOT = OUT-2204-KDLPORS                               
022601         MOVE OUT-2204-IDARTNR TO W-IDARTNR                               
022602         MOVE OUT-2204-KDLPORS TO W-KDLPORS                               
022603                                                                          
023000         PERFORM S11-WRITE-W2213701                                       
023600       END-IF                                                             
023700       PERFORM CA-LAES-SRT-FILE                                           
023800     END-PERFORM                                                          
023900     .                                                                    
024000     EJECT                                                                
024100 CA-LAES-SRT-FILE SECTION.                                                
024200****************************************************************          
024300*    LÄSER POSTER PÅ SORTERINGSFILEN                           *          
024400****************************************************************          
024500                                                                          
024600     RETURN SRT-FILE INTO OUT-2204-AREA                                   
024700       AT END MOVE YES TO SW-SRT-FILE-EOF                                 
024800     END-RETURN                                                           
024900     .                                                                    
025000                                                                          
025100******************************************************************        
025200*                                                                *        
025300*    AVSLUTNING                                                  *        
025400*    STÄNG ALLA FILER                                            *        
025500*     KRIV UT POSTSUMS RÄKNEVERK                                 *        
025600*                                                                *        
025700******************************************************************        
025800                                                                          
025900 Z-FINISH SECTION.                                                        
026000                                                                          
026100     CLOSE W2213701                                                       
026200           W2213702                                                       
026300                                                                          
026400     MOVE 'S' TO POSTSUM-OPKOD                                            
026500     CALL POSTSUM USING POSTSUM-PARM                                      
026600     .                                                                    
026700                                                                          
026892 S11-WRITE-W2213701 SECTION.                                              
026894                                                                          
026899     WRITE OUT-2204-RECORD FROM OUT-2204-AREA                             
026900                                                                          
026901     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
026902     MOVE 'W2213701' TO POSTSUM-FDNAMN                                    
026903     MOVE 'W22137D1' TO POSTSUM-DDNAMN2                                   
026910     CALL POSTSUM USING POSTSUM-PARM                                      
027000     .                                                                    
027100                                                                          
027110 S12-WRITE-W2213702 SECTION.                                              
027120                                                                          
027130     WRITE OUT-2203-RECORD FROM OUT-2203-AREA                             
027140                                                                          
027150     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
027160     MOVE 'W2213702' TO POSTSUM-FDNAMN                                    
027170     MOVE 'W22137D2' TO POSTSUM-DDNAMN2                                   
027180     CALL POSTSUM USING POSTSUM-PARM                                      
027190     .                                                                    
027191                                                                          
027200 IMS-GU-XXBJ01 SECTION.                                                   
027300                                                                          
027400     STRING 'WLXXBJ01(WDG3KEY  =' W-WDGXKEY-2203-X ')'                    
027500            DELIMITED BY SIZE INTO SSA1                                   
027600     MOVE '  GE' TO GOOD-STATUSCODES                                      
027700     CALL CBLTDLI USING GU XXJB-PCB DLI-IO-WDGX2203 SSA1                  
027800     MOVE XXJB-STATUS-CODE TO STATUS-WS                                   
027810     PERFORM IMS-STATUSCHECK                                              
027820     .                                                                    
027830                                                                          
027840 IMS-GNP-XXBJ11 SECTION.                                                  
027850                                                                          
027860     MOVE 'WLXXBJ11 ' TO SSA2                                             
027870     MOVE '  GE' TO GOOD-STATUSCODES                                      
027880     CALL CBLTDLI USING GNP XXJB-PCB DLI-IO-WDGX2204 SSA2                 
027890     MOVE XXJB-STATUS-CODE TO STATUS-WS                                   
027891     PERFORM IMS-STATUSCHECK                                              
027892     .                                                                    
027893                                                                          
027906 IMS-STATUSCHECK SECTION.                                                 
027907                                                                          
027908     SET STATUS-IX TO 1                                                   
027909     SEARCH GOOD-STATUS                                                   
027910       AT END                                                             
027920         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
027930           DELIMITED BY SIZE INTO ERROR-TEXT                              
027940         DISPLAY ERROR-TEXT                                               
027950         CALL FELLOG                                                      
027960       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
027970         CONTINUE                                                         
027980     END-SEARCH                                                           
027990     .                                                                    
