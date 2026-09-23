001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W6111300.                                                
001300*AUTHOR.         MÅNS SAMUELSSON.                                         
001400*DATE-WRITTEN.   93/08/13.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        LÄSER IGENOM W6D1 OCH SKRIVER FIL MED PRIORITERADE KOLLIN        
002000*        TILL HL                                                          
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003501     SKIP2                                                                
003502*          --- PRIORITERADE KOLLIN                                        
003510     SELECT W61113                     ASSIGN TO W61113D1.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004101     SKIP3                                                                
004102 FD  W61113                                                               
004103     RECORDING       F                                                    
004104     BLOCK CONTAINS  0.                                                   
004105     SKIP2                                                                
004110*01  POST -COPY W6111301 -PRE  UT13-  -L.                                 
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W6111300'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800*      --- VALID IDDC CODES                                               
004810*                                                                         
004820*01    -COPY WWDC99                                                       
004830       EJECT                                                              
004900 01  SPAR-ADLAGOMR               PIC S9(3)   VALUE +0 COMP-3.             
004910 01  SPAR-IDARTNR                PIC S9(9)   VALUE +0 COMP-3.             
004911 01  SLUT-POST.                                                           
004912     03  FILLER                  PIC X(5)    VALUE '*END*'.               
004913     03  POST-RAKNARE            PIC 9(3)    VALUE ZERO.                  
004914     03  FILLER                  PIC X(1)    VALUE '*'.                   
004920                                                                          
005000     EJECT                                                                
005100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005200 01  FILLER REDEFINES DAGENS-DATUM.                                       
005300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005600     EJECT                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006300     SKIP2                                                                
006400*    --- PARAMETRAR TILL ABEND                                            
006500                                                                          
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402 01  UT13-START-OCH-SLUT-POST    PIC X(24)   VALUE                        
007403                                 'UT13-START-OCH-SLUTPOST'.               
007404     SKIP2                                                                
007405 01  UT13-AREA-START-SLUT  -COPY W6111301 -L                              
007406                                                                          
007407 01  UT13-AREA-START             PIC X(24)   VALUE                        
007408                                 'UT13-AREA-START  '.                     
007409     SKIP2                                                                
007410                                                                          
007420*01  AREA -COPY W6111301     -PRE UT13-                                   
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
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
010400     03  W6INLA11 REDEFINES IO-AREA.                                      
010500*       05  -COPY W6D111                                                  
010600     03  W6INLA21 REDEFINES IO-AREA.                                      
010610*       05  -COPY W6D121                                                  
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011001     EJECT                                                                
011002*01  -COPY W0008  -PRE W6D1-                                              
011010     05  FILLER                  PIC X.                                   
011100     EJECT                                                                
011201 PROCEDURE DIVISION  USING W6D1-PCB.                                      
011210     ENTRY 'DLITCBL' USING W6D1-PCB.                                      
011300                                                                          
011500     SKIP2                                                                
011600     PERFORM A-INIT                                                       
011700     PERFORM IMS-GN-W6D1                                                  
011800     PERFORM UNTIL SEGMENT-SLUT                                           
011900       EVALUATE W6D1-SEG-NAME-FB                                          
012000         WHEN 'W6D111'                                                    
012002           MOVE ART-IDDC TO WS-IDDC                                       
012010           IF CDC-SE                                                      
012100             MOVE ART-ADLAGOMR TO SPAR-ADLAGOMR                           
012110             MOVE ART-IDARTNR  TO SPAR-IDARTNR                            
012120           END-IF                                                         
012200         WHEN 'W6D121'                                                    
012210           IF CDC-SE                                                      
012300             IF SPAR-ADLAGOMR = 10 OR 21 OR 22 OR 76                      
012310               IF RAD-FLPRIO = JA                                         
012320                 IF RAD-KDINLSTA = '   ' OR 'FPK' OR 'SAK'                
012330                   IF RAD-IDOKOLLI > +0                                   
012331                     IF RAD-FLDIVKLI = NEJ                                
012340                       PERFORM B-SKAPA-W61113                             
012341                     END-IF                                               
012350                   END-IF                                                 
012360                 END-IF                                                   
012370               END-IF                                                     
012380             END-IF                                                       
012381           END-IF                                                         
012390       END-EVALUATE                                                       
012391                                                                          
012392       PERFORM IMS-GN-W6D1                                                
012400                                                                          
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
013701                                                                          
013710     OPEN OUTPUT W61113                                                   
013720     MOVE '*PRIOPALL*' TO UT13-AREA-START-SLUT                            
013730     WRITE UT13-POST FROM UT13-AREA-START-SLUT                            
013800     SKIP2                                                                
013900     ACCEPT DAGENS-DATUM  FROM DATE                                       
014010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014200     .                                                                    
014300     EJECT                                                                
014310 B-SKAPA-W61113 SECTION.                                                  
014320                                                                          
014330     MOVE SPAR-IDARTNR TO UT13-IDARTNR                                    
014340     MOVE SPAR-ADLAGOMR TO UT13-ADLAGOMR                                  
014350     MOVE RAD-IDLEVNR-KOLLI TO UT13-IDLEVNR-KOLLI                         
014360     MOVE RAD-IDOKOLLI TO UT13-IDOKOLLI                                   
014361     MOVE RAD-KVINLART TO UT13-KVINLART                                   
014362     MOVE '*'          TO UT13-ASTERISK-1                                 
014363                          UT13-ASTERISK-2                                 
014364                          UT13-ASTERISK-3                                 
014365                          UT13-ASTERISK-4                                 
014366                          UT13-ASTERISK-5                                 
014367                          UT13-ASTERISK-6                                 
014368     ADD +1            TO POST-RAKNARE                                    
014370                                                                          
014380     PERFORM S11-SKRIV-W61113                                             
014390     .                                                                    
014391     EJECT                                                                
014400 Z-FINIT SECTION.                                                         
014500     MOVE SLUT-POST  TO   UT13-AREA-START-SLUT                            
014501     WRITE UT13-POST FROM UT13-AREA-START-SLUT                            
014510     CLOSE W61113                                                         
014601     SKIP2                                                                
014602     MOVE 'S' TO POSTSUM-OPKOD                                            
014610     CALL POSTSUM USING POSTSUM-PARM                                      
014700     .                                                                    
014901     EJECT                                                                
014902 S11-SKRIV-W61113 SECTION.                                                
014903     SKIP2                                                                
014904     WRITE UT13-POST FROM UT13-AREA                                       
014905                                                                          
014907     MOVE 'W61113' TO POSTSUM-FDNAMN                                      
014908     MOVE 'W61113D1' TO POSTSUM-DDNAMN2                                   
014909     CALL POSTSUM USING POSTSUM-PARM                                      
014910     .                                                                    
015100     EJECT                                                                
016002 IMS-GN-W6D1   SECTION.                                                   
016003     SKIP2                                                                
016004     CALL CBLTDLI USING GN W6D1-PCB DLI-IO-AREA                           
016005     MOVE W6D1-STATUS-CODE TO STATUS-WS                                   
016006     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
016007     PERFORM IMS-STATUSKONTROLL                                           
016010     .                                                                    
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
