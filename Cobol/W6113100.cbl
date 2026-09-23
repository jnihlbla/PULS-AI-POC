001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W6113100.                                                
001300*AUTHOR.         MÅNS SAMUELSSON.                                         
001400*DATE-WRITTEN.   93/01/20.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        KOMPLETTERAR AK-BELÄGGNINGSFILEN TILL VIOS MED KDINLOMR          
002000*                                                                         
002110*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- SB FIL FRÅN W61130                                         
003403     SELECT W61130                     ASSIGN TO W61131D1.                
003404     SKIP2                                                                
003405*          --- AK-BELÄGGNINGSFIL TILL VIOS                                
003410     SELECT W61131                     ASSIGN TO W61131D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W61130                                                               
004003     RECORDING       V                                                    
004004     BLOCK CONTAINS  0.                                                   
004005     SKIP2                                                                
004006*01  -COPY W6113001      -L.                                              
004007     SKIP2                                                                
004008*01  -COPY W6113002      -L.                                              
004009     SKIP3                                                                
004010 FD  W61131                                                               
004011     RECORDING       V                                                    
004012     BLOCK CONTAINS  0.                                                   
004013     SKIP2                                                                
004014*01  POST -COPY W6113101 -PRE  UT31-001-  -L.                             
004015     SKIP2                                                                
004020*01  POST -COPY W6113102 -PRE  UT31-002-  -L.                             
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W6113100'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004801                                                                          
004802 77  W61130-EOF-SW               PIC X       VALUE 'N'.                   
004810     88  END-OF-W61130                       VALUE 'J'.                   
004900     EJECT                                                                
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     SKIP2                                                                
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302 01  IN30-AREA-START             PIC X(24)   VALUE                        
007303                                 'IN30-AREA-START  '.                     
007304     SKIP2                                                                
007305 01  IN30-AREA.                                                           
007306     03  IN30-IDPTYP             PIC X(3).                                
007308     03  FILLER                  PIC X(100).                              
007309*01  FILLER -COPY W6113001      -PRE IN30-   -RED  IN30-AREA              
007310*01  FILLER -COPY W6113002      -PRE IN30-   -RED  IN30-AREA              
007311     EJECT                                                                
007312 01  UT31-AREA-START             PIC X(24)   VALUE                        
007313                                 'UT31-AREA-START  '.                     
007314     SKIP2                                                                
007315 01  UT31-AREA.                                                           
007316     03  UT31-IDPTYP             PIC X(3).                                
007318     03  FILLER                  PIC X(100).                              
007319*01  FILLER -COPY W6113101      -PRE UT31-   -RED  UT31-AREA              
007320*01  FILLER -COPY W6113102      -PRE UT31-   -RED  UT31-AREA              
007400     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008101     03  W-W6GXKEY-01-X.                                                  
008102         05  FILLER              PIC X(4)     VALUE '6005'.               
008103         05  W-IDDC              PIC X(2)     VALUE SPACE.                
008104         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
008105     03  W-W6GXKEY-11-X.                                                  
008110         05  W-ADINLOMR          PIC X(4)    VALUE SPACE.                 
008120         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010100     SKIP3                                                                
010200 01  DLI-IO-AREA.                                                         
010300     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
010401     SKIP3                                                                
010405     03  W6PLAA11 REDEFINES IO-AREA.                                      
010410*        05  -COPY W6GX6006                                               
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011001     EJECT                                                                
011002*01  -COPY W0008  -PRE PLAA-                                              
011010     05  FILLER                  PIC X.                                   
011100     EJECT                                                                
011201 PROCEDURE DIVISION  USING PLAA-PCB.                                      
011210     ENTRY 'DLITCBL' USING PLAA-PCB.                                      
011300                                                                          
011500     SKIP2                                                                
011600     PERFORM A-INIT                                                       
011710     PERFORM S01-LAES-W61130                                              
011800     PERFORM UNTIL END-OF-W61130                                          
011900       IF IN30-IDPTYP = '001'                                             
011910         MOVE IN30-001-W6113001 TO UT31-001-W6113101                      
011920       ELSE                                                               
011930         IF IN30-002-ADINLOMR = SPACE                                     
011931           MOVE IN30-002-W6113002 TO UT31-002-W6113102                    
011932           MOVE SPACE             TO UT31-002-KDINLOMR                    
011940         ELSE                                                             
012000           MOVE IN30-002-IDDC     TO W-IDDC                               
012010           MOVE IN30-002-ADINLOMR TO W-ADINLOMR                           
012100           PERFORM IMS-GU-PLAA11                                          
012110           MOVE IN30-002-W6113002 TO UT31-002-W6113102                    
012120*               --- INCL IDDC                                             
012200           MOVE 6006-KDINLOMR     TO UT31-002-KDINLOMR                    
012201         END-IF                                                           
012210       END-IF                                                             
012300       PERFORM S11-SKRIV-W61131                                           
012400                                                                          
012510       PERFORM S01-LAES-W61130                                            
012600     END-PERFORM                                                          
012700                                                                          
012800                                                                          
012900     PERFORM Z-FINIT                                                      
013000                                                                          
013100     MOVE ZERO TO RETURN-CODE                                             
013200     GOBACK                                                               
013300     .                                                                    
013400     EJECT                                                                
013500 A-INIT SECTION.                                                          
013601                                                                          
013610     OPEN INPUT  W61130                                                   
013701                                                                          
013710     OPEN OUTPUT W61131                                                   
013800     SKIP2                                                                
013900     ACCEPT DAGENS-DATUM  FROM DATE                                       
014010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014200     .                                                                    
014300     EJECT                                                                
014400 Z-FINIT SECTION.                                                         
014501     CLOSE W61130                                                         
014510           W61131                                                         
014601     SKIP2                                                                
014602     MOVE 'S' TO POSTSUM-OPKOD                                            
014610     CALL POSTSUM USING POSTSUM-PARM                                      
014700     .                                                                    
014801     EJECT                                                                
014802 S01-LAES-W61130  SECTION.                                                
014803     SKIP2                                                                
014804     READ W61130 INTO IN30-AREA                                           
014805     AT END                                                               
014807        SET END-OF-W61130 TO TRUE                                         
014808                                                                          
014809     NOT AT END                                                           
014810        MOVE 'W61130' TO POSTSUM-FDNAMN                                   
014811        MOVE 'W61131D1' TO POSTSUM-DDNAMN2                                
014812        MOVE IN30-IDPTYP TO POSTSUM-TRANSTYP                              
014813        CALL POSTSUM USING POSTSUM-PARM                                   
014814     END-READ                                                             
014820     .                                                                    
014901     EJECT                                                                
014902 S11-SKRIV-W61131 SECTION.                                                
014903     SKIP2                                                                
014904     IF UT31-IDPTYP = '001'                                               
014905       WRITE UT31-001-POST FROM UT31-AREA                                 
014906     ELSE                                                                 
014907       WRITE UT31-002-POST FROM UT31-AREA                                 
014908     END-IF                                                               
014909                                                                          
014910     MOVE UT31-IDPTYP TO POSTSUM-TRANSTYP                                 
014911     MOVE 'W61131' TO POSTSUM-FDNAMN                                      
014912     MOVE 'W61131D2' TO POSTSUM-DDNAMN2                                   
014913     CALL POSTSUM USING POSTSUM-PARM                                      
014920     .                                                                    
015100     EJECT                                                                
015800* --- IMS SEKTIONER ---                                                   
015900     SKIP3                                                                
016011 IMS-GU-PLAA11 SECTION.                                                   
016012     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-01-X ')'                      
016013          DELIMITED BY SIZE INTO SSA1                                     
016014     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-11-X ')'                      
016015          DELIMITED BY SIZE INTO SSA2                                     
016016     MOVE '  ' TO GODK-STATUSKODER                                        
016017     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA SSA1 SSA2                 
016018     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
016019     PERFORM IMS-STATUSKONTROLL                                           
016020     .                                                                    
016100     EJECT                                                                
016200 IMS-STATUSKONTROLL SECTION.                                              
016300     SKIP2                                                                
016400     SET STATUS-IX TO 1                                                   
016500     SEARCH GODK-STATUS                                                   
016600       AT END                                                             
016700         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
016800         DISPLAY FELTEXT                                                  
016900         CALL FELLOG                                                      
017000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017100         CONTINUE                                                         
017200     END-SEARCH                                                           
017300     .                                                                    
